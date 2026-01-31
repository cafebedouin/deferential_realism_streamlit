import streamlit as st
import subprocess
import os
import re
import time
from google import genai
from google.genai import types
from google.genai.errors import ClientError

class DRAuditOrchestrator:
    def __init__(self, api_key):
        self.client = genai.Client(api_key=api_key)
        # 2026 Model Routing for Tier 1 Reliability
        self.models = {
            "janitor": "gemini-2.0-flash-lite", # High TPM for cleaning/extraction
            "architect": "gemini-2.0-flash",     # Balanced for logic
            "essayist": "gemini-2.5-pro"         # Maximum reasoning for synthesis
        }
        self.protocols = {
            "uke_d": self._load_asset("uke_d.md"),
            "uke_c": self._load_asset("uke_c.md"),
            "uke_w": self._load_asset("uke_w.md"),
            "gen_prompt": self._load_asset("constraint_story_generation_prompt.md"),
            "template": self._load_asset("constraint_story_template.pl"),
            "linter": self._load_asset("structural_linter.py")
        }
        
        # Force-clear any potentially stale caches on startup
        try:
            for cache in self.client.caches.list():
                if "dr_audit" in cache.display_name:
                    self.client.caches.delete(name=cache.name)
                    st.sidebar.warning(f"Cleared stale cache: {cache.display_name}")
        except Exception as e:
            st.sidebar.error(f"Cache clearing failed: {e}")

        # Initialize Cache State
        if "cached_content_name" not in st.session_state:
            self._create_context_cache()

    def _load_asset(self, filename):
        if os.path.exists(filename):
            with open(filename, "r", encoding="utf-8") as f:
                return f.read()
        return ""

    def _create_context_cache(self):
        """Bakes protocols and the generation mission into a static cache."""
        # Embed mission here to avoid instruction conflict later
        full_context = f"""
        UKE_D PROTOCOL: {self.protocols['uke_d']}
        UKE_C PROTOCOL: {self.protocols['uke_c']}
        UKE_W PROTOCOL: {self.protocols['uke_w']}
        LINTER RULES: {self.protocols['linter']}
        MISSION: {self.protocols['gen_prompt']}
        """

        try:
            # For older APIs, all content must be in the 'user' role within the config.
            cache = self.client.caches.create(
                model=self.models["architect"],
                config=types.CreateCachedContentConfig(
                    display_name="dr_audit_vfinal",
                    contents=[types.Content(role="user", parts=[types.Part(text=full_context)])],
                    ttl="3600s"
                )
            )
            st.session_state.cached_content_name = cache.name
            st.sidebar.success("Context Cache Initialized.")
        except Exception as e:
            st.sidebar.error(f"Cache Initialization Failed: {e}")
            st.session_state.cached_content_name = None

    def get_cache_status(self):
        """Retrieves remaining TTL for the sidebar indicator."""
        if not st.session_state.get("cached_content_name"):
            return None
        try:
            cache_info = self.client.caches.get(name=st.session_state.cached_content_name)
            return cache_info.expire_time
        except Exception:
            st.session_state.cached_content_name = None
            return None

    def refresh_cache(self):
        """Forces a manual cache re-upload."""
        st.session_state.cached_content_name = None
        self._create_context_cache()

    def _gemini_call(self, system_instruction, user_content, role="architect"):
        """Decouples system_instruction when cache is present to avoid 400 error."""
        model_id = self.models.get(role, self.models["architect"])
        max_retries = 5
        base_delay = 2

        for attempt in range(max_retries):
            try:
                # Fully separate the logic for cached vs. non-cached calls
                if st.session_state.cached_content_name and role == "architect":
                    # Use cache, NO system instruction
                    config = types.GenerateContentConfig(
                        temperature=0.2, # Architect role uses lower temp
                        cached_content=st.session_state.cached_content_name
                    )
                    response = self.client.models.generate_content(
                        model=model_id,
                        contents=user_content,
                        config=config
                    )
                else:
                    # Normal call with system instruction, NO cache
                    config = types.GenerateContentConfig(
                        temperature=0.7 if role == "essayist" else 0.2,
                        system_instruction=system_instruction
                    )
                    response = self.client.models.generate_content(
                        model=model_id,
                        contents=user_content,
                        config=config
                    )

                return response.text
            except ClientError as e:
                if e.code == 429 and attempt < max_retries - 1:
                    wait_time = (base_delay * (2 ** attempt)) + (time.time() % 2)
                    st.warning(f"Rate limit hit. Retrying...")
                    time.sleep(wait_time)
                    continue
                raise e

    def run_pipeline(self, raw_input, max_retries=2):
        # 1. Extraction (Janitor)
        substrate = self._gemini_call(self.protocols["uke_d"], f"Extract anchors: {raw_input}", role="janitor")

        # 2. Flagging (Janitor)
        patterns = self._gemini_call(self.protocols["uke_c"], f"Flag patterns: {substrate}", role="janitor")

        # 3. Scenario Generation (Architect - CACHED)
        scenario_pl = None
        current_attempt = 0
        error_feedback = ""
        while current_attempt <= max_retries:
            retry_context = f"\n\nPREVIOUS_ERROR:\n{error_feedback}" if error_feedback else ""
            # The gen_prompt is now in the cache, but we pass it as a safety backup for non-cached calls
            system_msg = self.protocols['gen_prompt']

            scenario_pl = self._gemini_call(
                system_instruction=system_msg,
                user_content=f"Generate .pl using patterns.{retry_context}\nPATTERNS: {patterns}\nTEMPLATE:\n{self.protocols['template']}",
                role="architect"
            )

            linter_errors = self._lint_prolog(scenario_pl)
            if not linter_errors: break
            error_feedback = linter_errors
            current_attempt += 1
            time.sleep(1)

        if linter_errors: return f"### ❌ Linter Failed\n\n{linter_errors}"

        # 4. Logic Audit
        audit_output = self._execute_prolog(scenario_pl)

        # 5. Final Synthesis (Essayist)
        essay = self._gemini_call(
            self.protocols["uke_w"],
            f"SUBSTRATE: {substrate}\nAUDIT_LOG: {audit_output}\nSCENARIO_CODE: {scenario_pl}",
            role="essayist"
        )
        return essay

    def _lint_prolog(self, content):
        errors = []
        if not re.search(r':- module\(', content):
            errors.append("MISSING_MODULE: Prolog files must begin with :- module(id, []).")
        if "agent_power(individual_powerless)" not in content:
            errors.append("MISSING_PERSPECTIVE: Must include agent_power(individual_powerless).")
        found_types = set(re.findall(r'constraint_classification\(.*?,[\s\n\r]*(mountain|rope|snare|tangled_rope|scaffold|piton)', content))
        if len(found_types) < 2:
            errors.append(f"INSUFFICIENT_VARIANCE: Found {list(found_types)}. Need 2+ types.")
        return "\n".join([f"- {e}" for e in errors]) if errors else None

    def _execute_prolog(self, pl_code):
        temp_file = "temp_scenario.pl"
        with open(temp_file, "w", encoding="utf-8") as f:
            f.write(pl_code)
        try:
            cmd = ["swipl", "-q", "-s", temp_file, "-g", "run_audit", "-t", "halt"]
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=15)
            return result.stdout if result.returncode == 0 else result.stderr
        finally:
            if os.path.exists(temp_file):
                os.remove(temp_file)
