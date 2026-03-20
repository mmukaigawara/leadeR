# leadeR: Profiling Leaders at a Distance

The leadeR package profiles political leaders using text analysis, implementing Leadership Trait Analysis (LTA) and Operational Code Analysis (OCA). You provide text data and the package performs the analyses.

## Prerequisites

- **Python** (3.8 or later)
- **spaCy** Python library with an English language model installed
- **spacyr** R package (interface between R and spaCy)

To install spaCy and the English model:

```bash
pip install spacy
python -m spacy download en_core_web_sm
```

## Installation

You can install leadeR from GitHub:

```r
# install.packages("remotes")
remotes::install_github("mmukaigawara/leadeR")
```

## Usage

Before using leadeR functions, initialize spaCy:

```r
library(leadeR)
spacyr::spacy_initialize()
```

### Leadership Trait Analysis (LTA)

```r
# Analyze a leader's belief in their ability to control events
result <- get_power("Malawi", "We will help to improve relations with our neighbors.")
print(result)
```

### Operational Code Analysis (OCA)

```r
# Perform Operational Code Analysis on a speech
result <- get_opc("Malawi", "We will help to improve relations with our neighbors.")
print(result)
```

## Exported Functions

| Function | Description |
|---|---|
| `get_power()` | Belief in ability to control events (BACE) |
| `get_dist()` | Distrust of others |
| `get_nat()` | In-group bias / nationalism |
| `get_aff()` | Need for affiliation |
| `get_ctrl()` | Conceptual complexity (control) |
| `get_complex()` | Conceptual complexity |
| `get_task()` | Task orientation |
| `get_conf()` | Self-confidence |
| `get_opc()` | Operational Code Analysis |
