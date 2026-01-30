import subprocess
import os
from openai import OpenAI

class DRAuditOrchestrator:
    def __init__(self, api_key):
        self.client = OpenAI(api_key=api_key)
        # Load local protocol assets
        self.protocols = {
            "uke_d": open("uke_d.md").read(),
            "uke_c": open("uke_c.md").read(),
            "uke_w": open("uke_w.md").read(),
            "gen_prompt": open("constraint_story_generation_prompt.md").read(),
            "template": open("constraint_story_template.pl").read()
        }

    def run_pipeline(self, raw_input):
        # 1. Step 1 & 2: Search & UKE_D (Extraction)
        # Assuming raw_input is the text from a search result or user upload
        substrate = self._llm_call(self.protocols["uke_d"], f"Extract hard anchors: {raw_input}")

        # 2. Step 3: UKE_C (Pattern Flagging)
        patterns = self._llm_call(self.protocols["uke_c"], f"Flag patterns in: {substrate}")

        # 3. Step 4: Scenario Generation (.pl)
        scenario_pl = self._llm_call(
            self.protocols["gen_prompt"],
            f"Use template and patterns to generate .pl:\nPatterns: {patterns}\nTemplate: {self.protocols['template']}"
        )

        # 4. Step 5: Prolog Execution
        # Writes scenario to temp file and runs logic audit
        audit_output = self._execute_prolog(scenario_pl)

        # 5. Step 6: UKE_W (Synthesis)
        essay = self._llm_call(
            self.protocols["uke_w"],
            f"SUBSTRATE: {substrate}\nAUDIT: {audit_output}\nSCENARIO: {scenario_pl}"
        )
        return essay

    def _llm_call(self, system_prompt, user_content):
        res = self.client.chat.completions.create(
            model="gpt-4-turbo",
            messages=[{"role": "system", "content": system_prompt}, {"role": "user", "content": user_content}]
        )
        return res.choices[0].message.content

    def _execute_prolog(self, pl_code):
        with open("temp_scenario.pl", "w") as f:
            f.write(pl_code)
        # Assumes swipl is installed and prolog.txt logic is accessible
        cmd = ["swipl", "-q", "-s", "temp_scenario.pl", "-g", "run_audit", "-t", "halt"]
        result = subprocess.run(cmd, capture_output=True, text=True)
        return result.stdout
