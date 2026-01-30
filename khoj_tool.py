import os
from orchestrator import DRAuditOrchestrator

def dr_audit_tool(input_text: str) -> str:
    """
    Performs a Deferential Realism audit on the provided text.

    Use this tool when the user asks for a deep structural analysis,
    logical audit, or critical essay on complex scenarios like elections,
    policy changes, or institutional constraints.

    Input should be the scenario description or raw research data.
    """
    # 1. Retrieve the API Key from Khoj's environment variables
    api_key = os.getenv("GEMINI_API_KEY")
    if not api_key:
        return "Error: GEMINI_API_KEY not set in environment variables."

    try:
        # 2. Initialize the Orchestrator
        # This automatically loads the UKE_D, UKE_C, and UKE_W protocols
        orchestrator = DRAuditOrchestrator(api_key)

        # 3. Execute the Full Pipeline
        # This replaces the manual multi-function calls with the 5-step logic
        return orchestrator.run_pipeline(input_text)

    except Exception as e:
        return f"Audit Pipeline Error: {str(e)}"
