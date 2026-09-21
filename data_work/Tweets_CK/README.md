# Kern et al. (2023) Human Reference Annotations

This directory (`data_work/Tweets_CK/`) holds the human reference annotations from
Kern et al. (2023) used for comparison against the LLM annotations in the paper.

These reference annotations are **not** redistributed in this repository due to
licensing terms.

---

## Getting the Reference Data (Recommended)

The exact 44,900-annotation subset used by the pipeline is hosted in the original
replication repository for Kern et al. (2023):

> <https://github.com/chkern/tweet-annotation-sensitivity/tree/main/data>

You can download the two expected CSV files directly into this directory:

```bash
mkdir -p data_work/Tweets_CK

curl -sL https://raw.githubusercontent.com/chkern/tweet-annotation-sensitivity/main/data/full_train_s.csv \
  -o data_work/Tweets_CK/full_train_s.csv

curl -sL https://raw.githubusercontent.com/chkern/tweet-annotation-sensitivity/main/data/full_test_s.csv \
  -o data_work/Tweets_CK/full_test_s.csv
```

### Expected Fingerprint

When `process_raw_data.R` runs, it asserts that these two files match this exact shape:

| Metric | Expected Value |
|---|---|
| Total annotations (both files combined) | 44,900 |
| Train split (`full_train_s.csv`) | 33,683 |
| Test split (`full_test_s.csv`) | 11,217 |
| Distinct tweets | 3,000 |
| Distinct annotators | 917 |
| Questionnaire versions | A, B, C, D, E |

### Required Columns

The script expects both CSVs to carry at least:

| Column | Meaning |
|---|---|
| `tweet.id` | Tweet identifier, joins to the LLM annotations |
| `id` | Annotator identifier |
| `version` | Questionnaire version shown to the annotator (A–E) |
| `batch.tweet` | Position of the tweet within the annotator's batch |
| `hate.speech` | Human hate-speech label (0/1) |
| `offensive.language` | Human offensive-language label (0/1) |
| `tweet_hashed` | Tweet text with screen names masked |

`process_raw_data.R` concatenates these into `data_work/processed/kern_full.csv`.

---

## Alternative: Generating from the Hugging Face Dataset

You can also construct this exact 44,900 reference annotation subset from the full
public release on Hugging Face:

> <https://huggingface.co/datasets/soda-lmu/tweet-annotation-sensitivity-2>

The Hugging Face dataset contains the complete unpruned collection of $n=89,150$
crowdsourced annotations across 1,841 annotators and 3,050 tweets. The 44,900 subset
used in the paper corresponds to taking the first 3 annotations per tweet
(sorted by `annotator_id`) for each of the 5 questionnaire versions (A–E).

You can generate `data_work/processed/kern_full.csv` directly with this Python script:

```python
import pandas as pd
from datasets import load_dataset

# 1. Load the full Hugging Face dataset (89,150 rows)
ds = load_dataset("soda-lmu/tweet-annotation-sensitivity-2", split="train")
df = ds.to_pandas()

# 2. Filter valid rows and sort by condition, tweet_id, annotator_id
df_clean = df[
    df["tweet_hashed"].notna()
    & ~(df["hate_speech"].isna() & df["offensive_language"].isna())
].copy()
df_sorted = df_clean.sort_values(by=["condition", "tweet_id", "annotator_id"])

# 3. Retain the first 3 annotations per tweet per condition (yields exactly 44,900 rows)
df_44900 = df_sorted.groupby(["condition", "tweet_id"]).head(3).copy()

# 4. Standardize column names to match the pipeline
df_44900 = df_44900.rename(
    columns={
        "annotator_id": "id",
        "condition": "version",
        "tweet_batch": "batch_tweet",
    }
)

# 5. Normalize newline characters (\r\n -> \n)
df_44900["tweet_hashed"] = df_44900["tweet_hashed"].str.replace("\r\n", "\n")

# 6. Keep the required columns
cols = [
    "tweet_id",
    "id",
    "version",
    "batch_tweet",
    "hate_speech",
    "offensive_language",
    "tweet_hashed",
]
df_44900 = df_44900[cols]

# 7. Write directly to processed/kern_full.csv
df_44900.to_csv("data_work/processed/kern_full.csv", index=False)
print("Saved 44,900 reference annotations to data_work/processed/kern_full.csv")
```
