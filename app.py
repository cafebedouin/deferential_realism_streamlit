import streamlit as st
from orchestrator import DRAuditOrchestrator

st.set_page_config(page_title="DR-Audit Studio", layout="wide")
st.title("🛡️ Deferential Realism Audit Studio")

# Initialize Orchestrator
if "orchestrator" not in st.session_state:
    st.session_state.orchestrator = DRAuditOrchestrator(st.secrets["OPENAI_API_KEY"])

user_input = st.text_area("Paste research or scenario idea:")

if st.button("Run Full Audit"):
    with st.status("Executing Multi-Step Pipeline...", expanded=True) as status:
        st.write("🔍 Extracting Substrate (UKE_D)...")
        # pipeline logic...
        final_essay = st.session_state.orchestrator.run_pipeline(user_input)
        status.update(label="Audit Complete!", state="complete")

    st.markdown("### Final Essay")
    st.markdown(final_essay)
