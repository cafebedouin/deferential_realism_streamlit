import streamlit as st
from google import genai  # Modern 2026 SDK
from orchestrator import DRAuditOrchestrator

st.set_page_config(page_title="DR-Audit Studio", layout="wide")
st.title("🛡️ Deferential Realism Audit Studio")

# --- INITIALIZATION ---
# Using attribute notation for secrets is cleaner in Streamlit
if "GEMINI_API_KEY" not in st.secrets:
    st.error("Missing GEMINI_API_KEY in Streamlit Secrets.")
    st.stop()

# Initialize Orchestrator once per session
if "orchestrator" not in st.session_state:
    # Pass the key; the orchestrator's __init__ should handle genai.Client(api_key=...)
    st.session_state.orchestrator = DRAuditOrchestrator(st.secrets.GEMINI_API_KEY)

# --- UI ---
user_input = st.text_area("Paste research or scenario idea:", height=200)

if st.button("Run Full Audit", type="primary"):
    if not user_input.strip():
        st.warning("Please provide an input before running the audit.")
    else:
        with st.status("Executing Multi-Step Pipeline...", expanded=True) as status:
            st.write("🔍 Extracting Substrate (UKE_D)...")

            # The orchestrator handles the internal logic (D -> C -> PL -> W)
            final_essay = st.session_state.orchestrator.run_pipeline(user_input)

            status.update(label="Audit Complete!", state="complete")

        st.divider()
        st.markdown("### Final Essay")
        st.markdown(final_essay)
