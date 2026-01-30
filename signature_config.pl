/* ================================================================
   v3.2 STRUCTURAL SIGNATURE CONFIGURATION
   
   Add these parameters to config.pl to enable signature detection
   ================================================================ */

% NATURAL LAW SIGNATURE THRESHOLDS
% These identify constraints that are inherent properties of reality

param(natural_law_collapse_min, 0.85).
% Minimum accessibility collapse for natural law
% Natural laws affect everyone equally (high collapse)

param(natural_law_suppression_max, 0.15).
% Maximum suppression for natural law
% Natural laws don't require enforcement

param(natural_law_resistance_max, 0.15).
% Maximum resistance for natural law
% Natural laws cannot be effectively resisted

% COORDINATION SCAFFOLD SIGNATURE THRESHOLDS  
% These identify successful voluntary coordination standards

param(coordination_collapse_min, 0.85).
% Minimum accessibility collapse for coordination
% Successful standards achieve universal adoption

param(coordination_suppression_max, 0.15).
% Maximum suppression for coordination
% Coordination scaffolds are adopted voluntarily

param(coordination_resistance_max, 0.15).
% Maximum resistance for coordination
% Successful coordination faces minimal opposition

% CONSTRUCTED CONSTRAINT SIGNATURE THRESHOLDS
% These identify institutionally enforced rules

param(constructed_suppression_min, 0.20).
% Minimum suppression indicating institutional enforcement
% Constructed constraints require active maintenance

param(constructed_resistance_min, 0.20).
% Minimum resistance indicating opposition
% Constructed constraints face pushback

param(constructed_beneficiary_min, 2).
% Minimum beneficiary count for asymmetric gains
% Constructed constraints often benefit specific groups

/* ================================================================
   USAGE NOTES
   
   1. Natural Law vs Coordination Scaffold Distinction:
      Both have extreme collapse + minimal enforcement
      KEY DIFFERENCE: Coordination had viable alternatives
      
   2. Calibration Recommendations:
      - Start with these defaults
      - Monitor false positives in conceptual domains
      - Adjust collapse_min down if too restrictive
      - Adjust suppression_max up if missing constructs
      
   3. Integration with Existing System:
      - Structural signatures OVERRIDE modal classification
      - Use integrate_signature_with_modal/3 in report flow
      - Add signature explanations to audit output
   ================================================================ */
