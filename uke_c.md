# UKE_C v1.0 [Universal Knowledge Evaluator - Criticism Protocol]

**Protocol Version:** 1.0  
**Medium:** TEXT (Literary/Written Works)  
**Status:** Production  
**License:** CC0-1.0 (Public Domain)  
**Parent Suite:** UKE Protocol Suite

---

## §0. FOUNDATION

### Purpose
Evaluate creative work through rigorous observation, contextualization, and judgment.

### Core Invariants

**OBSERVATION > INTERPRETATION > JUDGMENT**
Never judge what you haven't seen. Never interpret what you haven't cataloged.

**THE WORK > THE THEORY**
Don't force-fit frameworks. Let the work determine which critical techniques are relevant.

**SIGNATURE MOVES ARE TECHNIQUES, NOT VOICES**
Deploy critical operations, don't imitate critics.

### Scope of Authority
Criticism may evaluate form, context, and achievement, but may **not** rewrite the work's intent to suit the critic's preferred narrative.

### The Gate Rule (Non-Negotiable)
**No move may be deployed without a corresponding pattern flag from §2.**

If §2 produces no flags, the work receives a **descriptive report only**—no evaluative critique. This prevents interpretive hallucination.

### Modes of Operation

**Standard Mode:** Full protocol execution (§1→§2→§3→§4→§5)  
**Audit Mode:** When reviewing another critic's work using UKE_C  
**Teaching Mode:** Step-by-step execution with explanation of choices

---

## §1. MATERIAL ENCOUNTER (TEXT)

**Purpose:** Catalog observable textual features without interpretation.

**Output Format:** Structured inventory that any hostile observer would confirm.

**Hostile Observer Standard:** Before proceeding to §2, you must be able to answer: *"What would someone who disagrees with me about this work's quality be forced to admit is present in the text?"*

---

### §1.1: STRUCTURAL INVENTORY

```markdown
[FORM & SCALE]
1. Genre signals present:
   - Paratextual markers (subtitle, cover copy, blurbs, epigraphs):
   - Opening conventions (in medias res, scene-setting, direct address):
   - Structural divisions (chapters, sections, fragments, continuous):

2. Length & density:
   - Total word count (if available):
   - Average paragraph length (count 3 random samples):
   - Average sentence length (count 3 random samples):

[NARRATIVE ARCHITECTURE]
3. Point of view:
   - Person (1st/2nd/3rd):
   - Number of POV characters:
   - Reliability markers (contradictions, admissions of bias, temporal gaps):

4. Temporal structure:
   - Chronological order: [Yes/No]
   - If non-linear, structure type: [Flashback/Fragmented/Parallel/Cyclical/Indeterminate]
   - Evidence: [Timestamps, date markers, tense shifts, explicit time references]

5. Spatial organization:
   - Single location / Multiple locations:
   - Geography explicitly named: [Yes/No - List if yes]
   - Physical movement tracked: [Yes/No - Describe if yes]
```

---

### §1.2: PATTERN-DETECTION INVENTORY

```markdown
[REPETITION TRACKING]
6. Recurring words/phrases:
   - Element: [exact phrase]
   - Frequency: [count]
   - Locations: [page/paragraph/line references]
   - Context notes: [surrounding material - quote 5 words before/after each instance]

7. Recurring images/objects:
   - Element: [specific object or image]
   - Frequency: [count]
   - Locations: [references]
   - Descriptive variation: [how is it described each time?]

8. Recurring structural patterns:
   - Pattern: [e.g., every chapter opens with weather, dialogue only in even sections]
   - Frequency: [count]
   - Consistency: [Perfect/Mostly/Breaks at location X]

[ABSENCE TRACKING]
9. Expected genre elements missing:
   - Typical for [genre]: [list standard conventions from 3+ comparison works]
   - Present in this work: [Yes/No for each]
   - Conspicuously absent: [what's missing that similar works include]

10. Narrative gaps:
    - Missing time: [Are there unexplained temporal jumps? Where?]
    - Missing information: [What facts are withheld? Examples:]
    - Missing voices: [Who would typically speak in this scenario but doesn't?]

[TENSION MARKERS]
11. Oppositional pairings:
    - Contrasting elements: [e.g., urban/rural, past/present, formal/colloquial]
    - Where they appear: [specific locations]
    - How they're juxtaposed: [within same sentence/paragraph/chapter]

12. Unresolved contradictions:
    - Statement A: [quote + location]
    - Statement B: [quote + location]
    - Relationship: [direct contradiction/tension/ambiguity/perspective difference]
```

---

### §1.3: SURFACE TEXTURE ANALYSIS

```markdown
[LANGUAGE REGISTER]
13. Diction inventory (count per 100 words in 3 random samples):
    - Latinate/academic terms:
    - Colloquialisms/slang:
    - Technical jargon:
    - Sensory language (sight/sound/touch/taste/smell):

14. Syntax patterns:
    - Dominant sentence type: [Simple/Compound/Complex/Fragment/Mixed]
    - Punctuation density: [Periods per 100 words / Other marks per 100 words]
    - Notable syntactic features: [Parentheticals, dashes, semicolons, etc.]

15. Tonal markers:
    - Emotional register: [Clinical/Lyrical/Sardonic/Neutral/Earnest - Evidence:]
    - Stability: [Consistent/Shifting - If shifting, note transitions with locations]

[FIGURATIVE LANGUAGE]
16. Metaphor/simile count: [Total in sample section]
    - Dominant source domains: [e.g., nature, machine, body, war]
    - Target domains: [what they describe]
    - Consistency: [Single metaphor system or mixed]

17. Allusion inventory:
    - Direct references: [Quote + page numbers]
    - Stylistic echoes: [Mimicry of other writers/genres - Evidence:]
```

---

### §1.4: META-TEXTUAL MARKERS

```markdown
[SELF-REFERENCE]
18. Text refers to its own composition: [Yes/No]
    - Evidence: [Quotes where narrator/character discusses writing, storytelling, etc.]

19. Text breaks its own frame: [Yes/No]
    - Evidence: [Direct address to reader, acknowledgment of fictionality, etc.]

20. Structural commentary:
    - Does form mirror content? [Example:]
    - Are there footnotes, appendices, or non-standard insertions? [List:]
```

---

## §2. PATTERN RECOGNITION (FLAG GENERATION)

**Purpose:** Convert raw inventory into **pattern flags** that justify move deployment.

**Rule:** A flag requires **minimum threshold evidence** from §1. Flags are binary: ACTIVE or INACTIVE.

---

### §2.1: FLAG DEFINITIONS & THRESHOLDS

```markdown
[FLAG: SYMBOL_CANDIDATE]
Threshold:
- Element appears ≥3 times in distinct contexts (§1.6 or §1.7)
- AND descriptive language or surrounding context shifts ≥1 time
- AND not explained as literal repetition (e.g., character's catchphrase, setting fixture)

Evidence Required:
- List element + all locations (page/paragraph)
- Quote context for each instance showing variation

Status: [ACTIVE / INACTIVE]

---

[FLAG: ABSENCE_STRUCTURAL]
Threshold:
- Genre convention present in 80%+ of comparison examples (§1.9)
- AND explicitly missing from this work
- OR narrative gap affects causality/chronology (§1.10)

Evidence Required:
- Name expected element
- Cite 3+ comparison works where it appears
- OR describe causal gap and its narrative impact

Status: [ACTIVE / INACTIVE]

---

[FLAG: META_GESTURE]
Threshold:
- Text explicitly references its own making (§1.18)
- OR breaks narrative frame (§1.19)
- OR form performs meaning (§1.20)

Evidence Required:
- Direct quote showing self-reference
- Location where frame breaks
- OR description of form-content mirroring

Status: [ACTIVE / INACTIVE]

---

[FLAG: REGISTER_COLLISION]
Threshold:
- Two+ distinct diction levels appear within single paragraph (§1.13)
- AND shift is not dialogue-based (different characters speaking)
- Measured by: 3+ Latinate terms + 3+ colloquialisms in same paragraph
  OR clinical language + lyrical language in same sentence

Evidence Required:
- Quote showing collision
- Identify each register with evidence from §1.13

Status: [ACTIVE / INACTIVE]

---

[FLAG: CONSTRAINT_FORMAL]
Threshold:
- Work adheres to externally-defined rule set (e.g., sonnet, lipogram, single sentence)
- OR self-imposed constraint mentioned in paratext/interviews
- Measured by: Structural consistency ≥90% (§1.8)

Evidence Required:
- Name constraint
- Show adherence across multiple instances (minimum 3)
- OR cite paratext stating constraint

Status: [ACTIVE / INACTIVE]

---

[FLAG: ALLUSION_NETWORK]
Threshold:
- Direct reference to ≥2 external texts (§1.17)
- OR sustained stylistic mimicry (≥1 paragraph length)

Evidence Required:
- Quote allusions with page numbers
- Identify sources
- OR describe stylistic mimicry with comparison quotes

Status: [ACTIVE / INACTIVE]

---

[FLAG: INTENSITY_HIGH]
Threshold:
- Sentence density >30 words avg in sustained section (≥2 paragraphs)
- OR metaphor rate >3 per 100 words (§1.16)
- OR punctuation density >15 marks per 100 words (§1.14)

Evidence Required:
- Quote dense section
- Provide counts from §1 inventory

Status: [ACTIVE / INACTIVE]

---

[FLAG: INTENSITY_LOW]
Threshold:
- Sentence avg <10 words in sustained section (≥2 paragraphs)
- OR paragraph breaks >1 per 50 words
- OR white space exceeds text in visual layout (for works where layout matters)

Evidence Required:
- Quote sparse section
- Describe visual effect or provide word counts

Status: [ACTIVE / INACTIVE]

---

[FLAG: CONTEXT_EXPLICIT]
Threshold:
- Text names specific historical event, date, or public figure (§1.17)
- OR setting details clearly locate work in specific historical moment

Evidence Required:
- Quote reference with page number
- Identify moment/context

Status: [ACTIVE / INACTIVE]

---

[FLAG: FAILURE_VISIBLE]
Threshold:
- Genre promise broken (§1.1 signals X, text delivers Y)
- OR technical error repeated ≥3 times (grammar, logic, consistency breaks)
- OR structural problem pattern (not isolated incidents)
- EXCLUDE: Character statements about failure (distinguish diegetic from formal failure)

Evidence Required:
- State promise (cite paratextual signals or opening conventions)
- Quote/cite failure instances with locations
- Argue why it's formal failure, not character trait or intentional choice

Status: [ACTIVE / INACTIVE]

---

[FLAG: TEMPORAL_COMPLEX]
Threshold:
- Non-linear chronology with ≥3 distinct time periods (§1.4)
- OR unreliable temporal markers (contradictory dates, impossible sequences)
- OR sustained temporal ambiguity (past/present blurred for ≥2 pages)

Evidence Required:
- Map timeline: list all temporal markers with locations
- Identify breaks in chronology
- Quote contradictions if present

Status: [ACTIVE / INACTIVE]

---

[FLAG: ETHICAL_PRESSURE]
Threshold:
- Work explicitly depicts harm, violence, or suffering
- OR engages representation of marginalized identities
- OR presents moral dilemma without easy resolution
- AND treatment of subject invites scrutiny of the work's ethical stance

Evidence Required:
- Quote/cite the ethical content
- Describe how work frames it (whose perspective, what distance)
- Note what's at stake in the representation

Status: [ACTIVE / INACTIVE]

---

[FLAG: GENRE_UNSTABLE]
Threshold:
- Work signals ≥2 distinct genres (§1.1)
- OR violates genre conventions in sustained way (≥3 instances)
- OR genre markers contradict each other

Evidence Required:
- List genre signals
- Cite violations of genre expectations
- Provide comparison works showing standard conventions

Status: [ACTIVE / INACTIVE]

---

[FLAG: AFFECTIVE_PRONOUNCED]
Threshold:
- Work provokes specific, strong emotion (dread, euphoria, disgust, etc.)
- AND affect is engineered through formal techniques (not just content)
- AND affect is sustained for ≥2 pages or returns ≥3 times

Evidence Required:
- Name the specific affect (be precise)
- Quote formal techniques from §1 that generate it
- Map where affect appears/sustains

Status: [ACTIVE / INACTIVE]

---

[FLAG: SCALE_DYNAMIC]
Threshold:
- Work moves between micro (intimate detail) and macro (systems, abstraction) ≥3 times
- OR holds both scales in sustained tension

Evidence Required:
- Identify minimum 2 distinct scales
- Quote examples of each with page numbers
- Map transitions between them

Status: [ACTIVE / INACTIVE]

---

[FLAG: VOICE_UNSTABLE]
Threshold:
- POV shifts ≥3 times without clear markers (§1.3)
- OR narrator makes contradictory statements (§1.12)
- OR reliability explicitly questioned in text (§1.3)

Evidence Required:
- Quote contradictions or map POV shifts
- Argue which type of unreliability (character/structural/reader-effect)

Status: [ACTIVE / INACTIVE]

---

[FLAG: MATERIAL_VISIBLE]
Threshold:
- Language itself becomes subject (§1.18)
- OR text calls attention to its materiality (print, page, typography)
- OR process of composition is foregrounded (§1.20)

Evidence Required:
- Quote where language/materiality is thematized
- Describe how foregrounding operates

Status: [ACTIVE / INACTIVE]

---

[FLAG: STRUCTURE_MIRRORS]
Threshold:
- Formal structure enacts or mirrors thematic content (§1.20)
- AND mirroring is sustained (not coincidental)

Evidence Required:
- State the thematic content
- Map the structural parallel with specific examples
- Argue why connection is meaningful

Status: [ACTIVE / INACTIVE]

---

[FLAG: INTERTEXT_EXPLICIT]
Threshold:
- Work explicitly responds to/rewrites specific prior work (beyond genre influence)
- OR sustained engagement with another text (≥3 references or entire structure based on source)

Evidence Required:
- Name source work(s)
- Quote/cite specific points of contact
- Describe relationship (homage/critique/correction/transformation)

Status: [ACTIVE / INACTIVE]

---

[FLAG: FAILURE_SYSTEMATIC]
Threshold:
- Same type of problem appears ≥3 times
- AND pattern suggests structural issue (not isolated errors)

Evidence Required:
- Identify the pattern (describe the repeated problem)
- Cite minimum 3 instances with locations
- Argue why it's systematic (not coincidental)
- Distinguish from intentional difficulty

Status: [ACTIVE / INACTIVE]
```

---

### §2.2: FLAG REPORT (OUTPUT)

```markdown
[PATTERN-FLAGS: SUMMARY]
Total Active: [N]
Total Inactive: [N]

[ACTIVE FLAGS]
✓ FLAG_NAME: [Brief evidence summary]
✓ FLAG_NAME: [Brief evidence summary]
...

[INACTIVE FLAGS]
✗ FLAG_NAME: [Why threshold not met]
✗ FLAG_NAME: [Why threshold not met]
...

[PATTERN-WEIGHT RANKING]
(If ≥4 flags active, rank by structural centrality for move selection)
1. FLAG_NAME (centrality: [high/medium/low])
2. FLAG_NAME (centrality: [high/medium/low])
...
```

---

## §3. MOVE DEPLOYMENT (ROUTING)

**Rule:** Deploy only moves whose pattern flags are ACTIVE.

**Conflict Resolution:**
- If 1-3 flags active: Deploy all corresponding moves
- If 4-6 flags active: Deploy top 3 by structural weight
- If 7+ flags active: Deploy top 4-5, note others in synthesis

**Structural Weight Criteria:**
- **High:** Pattern is central to work's architecture (appears throughout, ties to title/opening/closing, structurally load-bearing)
- **Medium:** Pattern is significant but localized (important to specific sections)
- **Low:** Pattern is present but peripheral (doesn't affect core reading experience)

---

### §3.1: ROUTING TABLE

| Active Flag | Primary Move | Secondary Move (Optional) | Priority |
|-------------|--------------|---------------------------|----------|
| SYMBOL_CANDIDATE | M01: Symbol Tracking | M05 if symbol is borrowed | High |
| ABSENCE_STRUCTURAL | M02: Absence Cataloging | M08 if absence is cultural | High |
| META_GESTURE | M03: Recursive Commentary | M04 if meta moment shifts tone | Medium |
| REGISTER_COLLISION | M04: Register Analysis | M07 if collision creates density | High |
| CONSTRAINT_FORMAL | M06: Constraint Diagnosis | — | Medium |
| ALLUSION_NETWORK | M05: Allusion Mapping | M19 if explicit dialogue | Medium |
| INTENSITY_HIGH | M07: Intensity Calibration | — | Medium |
| INTENSITY_LOW | M07: Intensity Calibration | M02 if restraint = absence | Medium |
| CONTEXT_EXPLICIT | M08: Context Triangulation | — | Medium |
| FAILURE_VISIBLE | M10: Failure Analysis | M20 if pattern detected | High |
| TEMPORAL_COMPLEX | M11: Temporal Mapping | — | High |
| ETHICAL_PRESSURE | M12: Ethical Pressure Test | M09 if relevant lived experience | High |
| GENRE_UNSTABLE | M13: Genre Deconstruction | — | High |
| AFFECTIVE_PRONOUNCED | M14: Affective Tracing | M18 if form=feeling | Medium |
| SCALE_DYNAMIC | M15: Scale Dynamics | — | Medium |
| VOICE_UNSTABLE | M16: Voice Instability | M11 if tied to time | High |
| MATERIAL_VISIBLE | M17: Material Foregrounding | M03 if reflexive | Medium |
| STRUCTURE_MIRRORS | M18: Structural Mirroring | — | Medium |
| INTERTTEXT_EXPLICIT | M19: Intertextual Dialogue | M05 for lineage | High |
| FAILURE_SYSTEMATIC | M20: Systematic Failure | M10 for pattern diagnosis | High |

---

## §4. MOVE EXECUTION (CRITICAL TECHNIQUES)

**For each flagged pattern, execute the corresponding move.**

---

### M01: SYMBOL TRACKING

**Purpose:** Map recurring element's transformation across text.

**Execution Steps:**
1. **List all instances** with page/paragraph references
2. **Chart semantic drift**: How does meaning/treatment change?
3. **Identify turning point**: Where does transformation become visible?
4. **Evaluate function**: Does drift serve narrative/thematic purpose or create confusion?

**Output Format:**
```markdown
[M01: SYMBOL TRACKING - "element name"]

Instances mapped:
- Location 1: [quote + context]
- Location 2: [quote + context]
...

Semantic drift:
Movement: [describe transformation arc]
Turning point: [location where change becomes clear]

Evaluation:
Does drift clarify theme? [Yes/No - evidence]
Or create confusion? [Yes/No - evidence]

[JUDGMENT]
[One-sentence verdict on whether symbol earns its recurrence]
```

---

### M02: ABSENCE CATALOGING

**Purpose:** Analyze the function of what's missing.

**Execution Steps:**
1. **Identify the gap**: What's absent that genre/convention would include?
2. **Map where absence matters**: Which moments would be clarified by the missing element?
3. **Distinguish types**: Intentional withholding vs. narrative failure
4. **Evaluate function**: What does the gap generate?

**Output Format:**
```markdown
[M02: ABSENCE CATALOGING - "missing element"]

Gap identified:
Expected element: [name it]
Genre precedent: [cite 3 comparison works]
Missing from: [describe scope]

Where absence matters:
- Location 1: [how gap affects reading]
- Location 2: [how gap affects reading]

Function determination:
Intentional withholding? [Yes/No - evidence]
Narrative failure? [Yes/No - evidence]

[JUDGMENT]
[Verdict on whether absence serves purpose]
```

---

### M03: RECURSIVE COMMENTARY

**Purpose:** Examine how work reflects on its own making.

**Execution Steps:**
1. **Quote self-referential moments**
2. **Analyze layers**: What does work say about itself?
3. **Evaluate integration**: Is meta-awareness earned or intrusive?

**Output Format:**
```markdown
[M03: RECURSIVE COMMENTARY]

Self-referential moments:
- Location: [quote + analysis]
- Location: [quote + analysis]

Layers identified:
Work's self-commentary: [what it says about itself]
Function: [Why call attention to construction?]

[JUDGMENT]
[Verdict on whether self-awareness deepens or distracts]
```

---

### M04: REGISTER ANALYSIS

**Purpose:** Diagnose tonal/stylistic collision function.

**Execution Steps:**
1. **Quote collision site**
2. **Identify each register** with evidence from §1
3. **Determine coherence**: Character-based? Authorial strategy? Error?
4. **Evaluate effect**: What does clash generate?

**Output Format:**
```markdown
[M04: REGISTER ANALYSIS]

Collision site: [quote]

Registers identified:
- High/Formal: [evidence]
- Low/Colloquial: [evidence]

Coherence check:
Character trait? [Yes/No - evidence]
Authorial tone? [Yes/No - evidence]
Effect: [describe what clash accomplishes]

[JUDGMENT]
[Verdict on whether collision serves purpose]
```

---

### M05: ALLUSION MAPPING

**Purpose:** Trace lineage and transformation of borrowed material.

**Execution Steps:**
1. **Identify all references** with locations
2. **Name sources**
3. **Determine relationship**: Honor? Critique? Transform?
4. **Evaluate use**: Does borrowing add insight?

**Output Format:**
```markdown
[M05: ALLUSION MAPPING]

References cataloged:
- Source: [name]
  Location in text: [quote]
  Relationship: [homage/critique/transformation]

Lineage traced:
Work's position in tradition: [describe]
What borrowing accomplishes: [analyze]

[JUDGMENT]
[Verdict on whether allusions enrich or obscure]
```

---

### M06: CONSTRAINT DIAGNOSIS

**Purpose:** Test whether formal limitation forces invention.

**Execution Steps:**
1. **Name the constraint**
2. **Show adherence** across multiple instances
3. **Analyze inventions**: What solutions does constraint force?
4. **Evaluate**: Gimmick or genuine innovation?

**Output Format:**
```markdown
[M06: CONSTRAINT DIAGNOSIS]

Constraint identified: [name it]
Evidence of adherence:
- Instance 1: [location + description]
- Instance 2: [location + description]

Inventions forced by constraint:
- [Describe novel solutions]

[JUDGMENT]
Innovation or gimmick? [Verdict with evidence]
```

---

### M07: INTENSITY CALIBRATION

**Purpose:** Measure saturation or deprivation effect.

**Execution Steps:**
1. **Quantify intensity** using §1 data (sentence length, metaphor rate, etc.)
2. **Describe effect** on reading experience
3. **Analyze purpose**: Does intensity serve the work's aims?
4. **Assess sustainability**: Can it be maintained? Should it be?

**Output Format:**
```markdown
[M07: INTENSITY CALIBRATION]

Intensity measured:
- [HIGH/LOW]
- Evidence: [specific counts from §1]

Effect on reading:
[Describe experiential impact]

Purpose analysis:
Justification: [Yes/No - why this density/sparseness?]
Sustainability: [Can work maintain this? Does it need to?]

[JUDGMENT]
[Verdict on whether intensity serves work]
```

---

### M08: CONTEXT TRIANGULATION

**Purpose:** Place work in historical/cultural moment.

**Execution Steps:**
1. **Identify the moment** work engages
2. **Map cultural pressures** visible in text
3. **Analyze response**: Does work absorb, resist, or transform its moment?

**Output Format:**
```markdown
[M08: CONTEXT TRIANGULATION]

Moment identified:
Historical context: [specific time/event]
Evidence in text: [quotes/details]

Cultural pressures visible:
- Pressure 1: [describe]
- Pressure 2: [describe]

Work's response:
[Absorb/Resist/Transform - with evidence]

[JUDGMENT]
[Verdict on work's relationship to its moment]
```

---

### M09: PERSONAL WITNESS

**Purpose:** Bring lived/insider knowledge to bear.

**Execution Steps:**
1. **Identify relevant lived experience** (critic's or community's)
2. **Test accuracy**: Does work ring true to those who've lived this?
3. **Note what insider perspective reveals** that outsider might miss
4. **Avoid gatekeeping**: Distinguish between "gets it wrong" and "not for this audience"

**Output Format:**
```markdown
[M09: PERSONAL WITNESS]

Relevant lived experience: [identify without over-sharing]

Accuracy test:
Does work ring true? [Yes/No/Partial - specific evidence]
What gets right: [cite]
What misses: [cite]

Insider insight:
What this perspective reveals: [analysis]

[JUDGMENT]
[Verdict on work's treatment of lived experience]
```

**Risk Warning:** This move is uniquely prone to gatekeeping ("only X can judge this"). Use only when lived experience genuinely illuminates something textually present.

---

### M10: FAILURE ANALYSIS

**Purpose:** Diagnose gap between intent and execution.

**Execution Steps:**
1. **State the promise** (from paratext, genre signals, or opening)
2. **Cite the failure** (where execution breaks promise)
3. **Be specific**: What exactly breaks down?
4. **Distinguish**: Failure vs. intentional difficulty vs. mismatched expectations

**Output Format:**
```markdown
[M10: FAILURE ANALYSIS]

Promise stated:
[What work signals it will do]
Evidence: [cite paratext, opening conventions]

Failure cited:
- Instance 1: [quote + location]
- Instance 2: [quote + location]

Breakdown diagnosis:
What specifically fails: [describe]
Why it fails: [analyze]

Type determination:
[ ] Execution failure (work can't do what it promises)
[ ] Intentional difficulty (work succeeds at hard thing)
[ ] Expectation mismatch (work does different thing than promised)

[JUDGMENT]
[Verdict on whether failure is fatal or productive]
```

---

### M11: TEMPORAL MAPPING

**Purpose:** Chart chronology vs. discourse to understand reordering.

**Execution Steps:**
1. **Create two timelines**: Story order (chronological) vs. Discourse order (as presented)
2. **Map each section** to chronological position
3. **Identify reordering logic**: Why this sequence?
4. **Evaluate effect**: Does scrambling serve purpose?

**Output Format:**
```markdown
[M11: TEMPORAL MAPPING]

Timeline 1 - Story Order (Chronological):
- Event A (actual first): [describe]
- Event B (actual second): [describe]
...

Timeline 2 - Discourse Order (As Presented):
- Section 1 presents: Event C (middle of story)
- Section 2 presents: Event A (flashback)
...

Reordering logic:
Why this sequence? [analyze purpose]

[JUDGMENT]
[Verdict on whether temporal complexity serves narrative]
```

---

### M12: ETHICAL PRESSURE TEST

**Purpose:** Evaluate work's ethical framework and effects.

**Execution Steps:**
1. **Identify ethical question** work raises
2. **Map work's positioning**: Whose perspective is centered? Whose pain matters?
3. **Test for evasion**: Where does work duck responsibility?
4. **Distinguish**: Ethical content (what's depicted) vs. Ethical stance (how framed) vs. Ethical effects (what's asked of reader)

**Output Format:**
```markdown
[M12: ETHICAL PRESSURE TEST]

Ethical question identified:
[State the moral dilemma or representational issue]
Evidence: [quote/cite]

Work's positioning:
Whose perspective centered: [analyze]
Whose pain/experience foregrounded: [analyze]
Distance/proximity to suffering: [close-up or distant?]

Evasions detected:
- Where work avoids responsibility: [cite]

Framework test:
Content: [what's depicted]
Stance: [how it's framed]
Effects: [what's asked of reader]

[JUDGMENT]
[Verdict on work's ethical coherence and honesty]
```

**Risk Warning:** Avoid demanding work be different work. Assess what it attempts, not what you wish it attempted.

---

### M13: GENRE DECONSTRUCTION

**Purpose:** Analyze how work uses/subverts genre conventions.

**Execution Steps:**
1. **Name genre expectations** (cite conventions from 3+ comparison works)
2. **Map where work fulfills conventions**
3. **Map where work breaks conventions**
4. **Evaluate breaks**: Generative or just confusing?

**Output Format:**
```markdown
[M13: GENRE DECONSTRUCTION]

Genre contract established:
Genre: [name it]
Standard conventions: [list from comparison works]

Fulfillment mapped:
- Convention kept: [cite example]
- Convention kept: [cite example]

Violations mapped:
- Convention broken: [cite + location]
- Effect of break: [analyze]

[JUDGMENT]
[Verdict on whether breaks generate insight or just confusion]
```

---

### M14: AFFECTIVE TRACING

**Purpose:** Correlate formal techniques with emotional effects.

**Execution Steps:**
1. **Name the affect** (be specific: not "sad," but "elegiac melancholy")
2. **Quote formal techniques** that generate it (from §1: rhythm, word choice, syntax)
3. **Track affect arc**: Where does it appear, sustain, shift?
4. **Distinguish**: Intended effect vs. accidental effect

**Output Format:**
```markdown
[M14: AFFECTIVE TRACING]

Affect named: [specific emotion]

Formal techniques generating affect:
- Technique 1: [quote evidence from §1]
- Technique 2: [quote evidence from §1]

Affective arc:
- Pages X-Y: [affect sustained/building]
- Page Z: [affect shifts to...]

Intent determination:
Intended effect: [Yes/No - how can you tell?]

[JUDGMENT]
[Verdict on whether affect is earned or manipulative]
```

**Risk Warning:** Distinguish between universal effects and personal reactions. Not all readers feel the same way.

---

### M15: SCALE DYNAMICS

**Purpose:** Analyze micro/macro tension.

**Execution Steps:**
1. **Identify scales** (minimum 2: intimate detail vs. large systems)
2. **Quote examples** of each scale
3. **Map transitions**: How does work move between scales?
4. **Evaluate tension**: What insight comes from scale play?

**Output Format:**
```markdown
[M15: SCALE DYNAMICS]

Scales identified:
- Micro: [quote example + location]
- Macro: [quote example + location]

Transitions mapped:
- Shift at page X: [from micro to macro - describe]
- Shift at page Y: [from macro to micro - describe]

Tension analysis:
What scale play generates: [insight/confusion?]
Does work privilege one scale? [Yes/No]

[JUDGMENT]
[Verdict on whether scalar shifts serve purpose]
```

---

### M16: VOICE INSTABILITY

**Purpose:** Map perspective shifts and contradictions.

**Execution Steps:**
1. **Map whose voice/perspective controls each section**
2. **Track contradictions** or lies
3. **Determine type**: Character unreliability? Structural unreliability? Reader-effect?
4. **Evaluate revelation**: What does instability expose?

**Output Format:**
```markdown
[M16: VOICE INSTABILITY]

POV mapping:
- Section 1: [whose perspective + evidence]
- Section 2: [whose perspective + evidence]

Contradictions tracked:
- Statement A: [quote + location]
- Statement B: [quote + location - contradicts A]

Unreliability type:
[ ] Character (they're lying/mistaken)
[ ] Structural (work itself is unstable)
[ ] Reader-effect (we misread due to bias)

[JUDGMENT]
[Verdict on what instability reveals]
```

---

### M17: MATERIAL FOREGROUNDING

**Purpose:** Analyze how work calls attention to its medium.

**Execution Steps:**
1. **Quote where material/process is foregrounded**
2. **Determine type**: Reflexive (thinking about art)? Materialist (physical reality)? Deconstructive (exposing illusion)?
3. **Evaluate effect**: Does exposure serve purpose?

**Output Format:**
```markdown
[M17: MATERIAL FOREGROUNDING]

Foregrounding moments:
- Location: [quote showing medium/process visible]

Type determination:
[ ] Reflexive (art about art-making)
[ ] Materialist (emphasizing physical reality of language)
[ ] Deconstructive (exposing fictional illusion)

Effect:
What exposure accomplishes: [analyze]

[JUDGMENT]
[Verdict on whether foregrounding adds depth or just cleverness]
```

---

### M18: STRUCTURAL MIRRORING

**Purpose:** Evaluate form-content parallel.

**Execution Steps:**
1. **State thematic content** (what work is "about")
2. **Map structural parallel** (how form enacts content)
3. **Evaluate elegance**: Deepens meaning? Heavy-handed? Coincidental?

**Output Format:**
```markdown
[M18: STRUCTURAL MIRRORING]

Content stated:
Thematic concern: [what work is about]

Structural parallel:
How form mirrors content: [specific examples]

Evaluation:
[ ] Elegant (deepens meaning)
[ ] Heavy-handed (obvious)
[ ] Coincidental (no real connection)

[JUDGMENT]
[Verdict on whether mirroring adds insight]
```

---

### M19: INTERTEXTUAL DIALOGUE

**Purpose:** Analyze explicit engagement with prior work.

**Execution Steps:**
1. **Name source work(s)**
2. **Map specific points of contact** (quotes, scenes, structures borrowed/transformed)
3. **Determine relationship**: Homage? Critique? Correction? Transformation?
4. **Evaluate dialogue**: What emerges from conversation?

**Output Format:**
```markdown
[M19: INTERTEXTUAL DIALOGUE]

Source work: [name + author]

Points of contact:
- Location in this text: [quote]
  Location in source: [comparison]
  Relationship: [homage/critique/etc.]

Dialogue analysis:
What conversation accomplishes: [insight that neither work alone produces]

[JUDGMENT]
[Verdict on whether dialogue enriches or just references]
```

---

### M20: SYSTEMATIC FAILURE ANALYSIS

**Purpose:** Diagnose repeated breakdown patterns.

**Execution Steps:**
1. **Identify the pattern** (same problem ≥3 times)
2. **Cite instances** with locations
3. **Argue systematicity**: Why is this structural, not coincidental?
4. **Distinguish**: Failure vs. intentional difficulty

**Output Format:**
```markdown
[M20: SYSTEMATIC FAILURE ANALYSIS]

Pattern identified:
Repeated problem: [describe]

Instances cited:
- Location 1: [quote/describe]
- Location 2: [quote/describe]
- Location 3: [quote/describe]

Systematicity argument:
Why this is structural: [analyze pattern]

Type determination:
[ ] Systematic failure (author can't execute)
[ ] Intentional difficulty (hard thing done on purpose)
[ ] Misdiagnosis (not actually a problem)

[JUDGMENT]
[Verdict on nature and severity of pattern]
```

---

## §5. EVALUATIVE SYNTHESIS

**Purpose:** Integrate observations into coherent critical judgment.

**Question Sequence:**
1. **Achievement:** Does this work accomplish what it attempts?
2. **Stakes:** Why does this matter (or not) beyond the work itself?
3. **Ω Elevation:** What remains genuinely unresolvable?

---

### §5.1: SYNTHESIS STRUCTURE

```markdown
[CRITICAL-ESSAY]

{Opening paragraph: State what work attempts and your provisional verdict}

{Body paragraphs: Present deployed moves with evidence, building toward argument}

{Each move should produce 1-3 paragraphs showing:
- Pattern observed (from §1)
- Interpretation (from move execution)
- Evaluation (does it work?)}

{Synthesis: How do multiple patterns/moves interact?}

[ACHIEVEMENT]
Does work accomplish its aims? [Yes/Partial/No]
Evidence: [cite specific moments of success/failure]

[STAKES]
Cultural significance: [Why this matters now]
Formal contribution: [What this adds to tradition/genre]

[LIMITATIONS]
Where work doesn't succeed: [be specific]
What's left unresolved: [distinguish productive from failure]

{Closing: Return to verdict with nuance}
```

---

### §5.2: Ω ELEVATION

**Purpose:** Convert unresolvable tensions into productive questions.

**Rules:**
- Ω questions are **bounded** (specific), not vague doubts
- They emerge from **textual evidence**, not critic's ideology
- They represent **genuine irresolvability**, not lack of effort

**Format:**
```markdown
[Ω: UNRESOLVED QUESTIONS]

Ω: [Label] — [Specific question the work raises but doesn't settle]
   Source: [Which move/observation generated this]

Ω: [Label] — [Specific question]
   Source: [Which move/observation]
```

**Examples:**
```markdown
Ω: Authorial Distance — Is the narrator-seamstress the author's surrogate, or is that identification itself being critiqued?
   Source: M03 (Recursive Commentary)

Ω: Cultural Specificity — Does this work require knowledge of specific textile traditions, or is the craft symbolism universal?
   Source: M01 (Symbol Tracking)
```

---

## §6. OUTPUT FORMAT

```markdown
[UKE_META]
protocol: UKE_C v1.0
work_evaluated: [Title, Author, Year, Genre]
word_count: [if known]
access: [Complete text / Excerpt / Translation]
timestamp: [ISO-8601]

[§1: MATERIAL-SCAN]
{Collapsed inventory - full version in working notes}
Key observations:
- [3-5 most significant patterns from §1]

[§2: PATTERN-FLAGS]
Total Active: [N]

✓ FLAG_NAME: [Element - frequency - evidence summary]
✓ FLAG_NAME: [Element - frequency - evidence summary]
...

[§3: MOVE-DEPLOYMENT]
Moves executed: [M##, M##, M##]
Priority ranking: [List with rationale if needed]

[§4: CRITICAL-ESSAY]
{Full synthesis as structured in §5.1}

[§5: Ω - UNRESOLVED]
Ω: [Label] — [Question]
Ω: [Label] — [Question]

[LOG]
tier: 3
confidence: 0.XX (H/M/L bin)
flags_active: [count]
flags_total: [count]
moves_deployed: [M## list]
checksum: [4-char or UNAVAIL]
```

---

## §7. USAGE INSTRUCTIONS

### Activation

**Standard activation:**
```
"Operate under UKE_C v1.0 (TEXT). Evaluate the following work:"
[paste text or provide citation]
```

**With constraints:**
```
"Operate under UKE_C v1.0 (TEXT).
Constraints: [specify any - e.g., "Focus on first 3 chapters only"]
Evaluate: [work]"
```

---

### Workflow

1. **Execute §1 (Material Encounter)**: Complete full inventory without interpretation
2. **Execute §2 (Flag Generation)**: Test all flags, mark ACTIVE/INACTIVE
3. **Execute §3 (Routing)**: Identify which moves to deploy
4. **Execute §4 (Move Execution)**: Run each move with evidence
5. **Execute §5 (Synthesis)**: Integrate into essay + Ω questions
6. **Execute §6 (Output)**: Format complete report

---

### Quality Checks

**Before proceeding from §1 to §2:**
- [ ] Can hostile observer confirm every §1 claim?
- [ ] Have I interpreted anything or just cataloged?
- [ ] Are all quotes accurate with locations?

**Before proceeding from §2 to §3:**
- [ ] Does every ACTIVE flag meet minimum threshold?
- [ ] Have I provided required evidence for each flag?
- [ ] Are any flags aspirational (hope to use move) vs. evidence-based?

**Before finalizing §5:**
- [ ] Does every claim in essay trace back to §1 evidence?
- [ ] Have I deployed only flagged moves?
- [ ] Are Ω questions specific and text-derived?

---

## §8. ANTI-PATTERNS & WARNINGS

### Common Failures

**F-PREMATURE-INTERPRETATION**
Moving to interpretation before completing §1 inventory.
**Fix:** Complete all §1 sections before any evaluative language.

**F-FLAG-INFLATION**
Marking flags ACTIVE when thresholds not met because you want to use the move.
**Fix:** Trust the gate rule. No flags = descriptive report is legitimate outcome.

**F-MOVE-SMUGGLING**
Deploying moves without corresponding flags.
**Fix:** Check §3 routing table. If no flag, no move.

**F-THEORY-IMPORT**
Bringing in critical theory not derived from textual patterns.
**Fix:** Every claim must trace to §1 evidence. If pattern isn't there, theory doesn't apply.

**F-VAGUE-OMEGA**
Ω questions that are philosophical musings, not text-specific irresolvables.
**Fix:** Ω must name the specific textual crux that remains unresolved.

**F-HOSTILE-OBSERVER-FAIL**
Stating "observations" that another critic couldn't verify.
**Fix:** If it's disputable, it's interpretation. Move it to §4 with evidence.

---

### Guardrails

**Resist the urge to:**
- Use every move on every work (flags determine moves)
- Import favorite theories regardless of text (work > theory)
- Praise/condemn based on personal taste (flag patterns, don't moralize)
- Demand work be different work (assess what it attempts)
- Over-read symbols (threshold exists to prevent hallucination)
- Under-read complexity (if pattern is there, flag it)

**Remember:**
- Criticism is diagnostic, not prosecutorial
- Flags protect against over-reading AND under-reading
- Ω questions are victories, not failures (genuine complexity acknowledged)
- Descriptive report (no flags) is legitimate outcome for some works

---

## §9. SKILL METADATA

**Integration with UKE Suite:**
- Can feed to **UKE_A (Audit)** for protocol compliance verification
- Can feed to **UKE_R (Review)** for governance decisions
- Output format compatible with UKE ecosystem

**Version History:**
- v0.1: Initial architecture (§0-§3)
- v0.2: TEXT protocol expansion (§1 detailed, §2 flags formalized)
- v1.0: Complete integration (20 moves, full workflow, anti-patterns)

**Cross-Media Status:**
- TEXT: Complete (this document)
- FILM: Drafted (§1-§2 validated)
- MUSIC: Drafted (§1-§2 validated)
- IMAGE: Drafted (§1-§2 validated)

---

## APPENDIX A: QUICK REFERENCE

### Flag → Move Routing (Condensed)

| Flag | Move | Priority |
|------|------|----------|
| SYMBOL_CANDIDATE | M01 | High |
| ABSENCE_STRUCTURAL | M02 | High |
| META_GESTURE | M03 | Medium |
| REGISTER_COLLISION | M04 | High |
| ALLUSION_NETWORK | M05 | Medium |
| CONSTRAINT_FORMAL | M06 | Medium |
| INTENSITY_HIGH/LOW | M07 | Medium |
| CONTEXT_EXPLICIT | M08 | Medium |
| FAILURE_VISIBLE | M10 | High |
| TEMPORAL_COMPLEX | M11 | High |
| ETHICAL_PRESSURE | M12 | High |
| GENRE_UNSTABLE | M13 | High |
| AFFECTIVE_PRONOUNCED | M14 | Medium |
| SCALE_DYNAMIC | M15 | Medium |
| VOICE_UNSTABLE | M16 | High |
| MATERIAL_VISIBLE | M17 | Medium |
| STRUCTURE_MIRRORS | M18 | Medium |
| INTERTTEXT_EXPLICIT | M19 | High |
| FAILURE_SYSTEMATIC | M20 | High |

---

### Threshold Quick Reference

**For SYMBOL_CANDIDATE:** ≥3 instances + context shift
**For ABSENCE_STRUCTURAL:** 80% genre convention missing
**For REGISTER_COLLISION:** 3+ high + 3+ low terms in same paragraph
**For INTENSITY_HIGH:** >30 word avg sentence OR >3 metaphors/100w
**For INTENSITY_LOW:** <10 word avg sentence
**For most others:** ≥3 instances or sustained pattern

---

## END OF UKE_C v1.0 (TEXT)

**Protocol Status:** Production-ready
**Next Step:** Apply to test work
**Recommended Test:** 2-3 page literary excerpt (high-quality prose with clear patterns)
