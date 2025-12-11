# Android Malware Classification Project

A Quarto website for analyzing and classifying Android malware using random forest models.

## Project Structure

```
ProjectProposal/
├── _quarto.yml              # Quarto website configuration
├── index.qmd                # Home page
├── preprocessing.qmd        # Data loading & feature engineering
├── category.qmd             # Category prediction model
├── family.qmd               # Family prediction models (category-specific)
├── proposal.qmd             # Exploratory Data Analysis
├── data/
│   ├── raw/                 # Original data (CSV files)
│   ├── processed/           # Processed datasets (RDS files)
│   └── models/              # Trained models (RDS files)
├── plots/                   # Saved visualizations
└── scripts/                 # Utility scripts
```

## Setup

1. Install required R packages:
   ```r
   install.packages(c("tidyverse", "ranger", "caret", "viridis", "ggalluvial"))
   ```

2. Install Quarto (if not already installed):
   - Visit https://quarto.org/docs/get-started/

## Data Requirements

The project expects data files in `data/raw/AndMal2020-dynamic-BeforeAndAfterReboot/` with pattern `*_Cat.csv`.

## Usage

### Rendering the Website

From the `ProjectProposal/` directory:

```bash
quarto render
```

This will:
1. Run `preprocessing.qmd` to load and process data
2. Execute modeling documents (`category.qmd`, `family.qmd`)
3. Generate visualizations in `proposal.qmd`
4. Build the website in `docs/`

### Individual Document Rendering

To render a single document:

```bash
quarto render preprocessing.qmd
quarto render category.qmd
quarto render family.qmd
quarto render proposal.qmd
```

### Preprocessing

Run `preprocessing.qmd` first to:
- Load raw CSV files
- Perform feature engineering
- Save processed datasets to `data/processed/`

This must be run before other documents that depend on processed data.

## Model Training

- **Category Prediction**: Trains a random forest to predict malware category
- **Family Prediction**: Trains separate models for Adware, Riskware, and Trojan families

Models are saved to `data/models/` after training.

## Output

- **Website**: Built in `docs/` directory (suitable for GitHub Pages)
- **Models**: Saved as `.rds` files in `data/models/`
- **Processed Data**: Saved as `.rds` files in `data/processed/`

## GitHub Pages Deployment

1. Configure repository to publish from `docs/` directory
2. The `docs/` folder contains the rendered HTML files
3. Ensure `.nojekyll` file exists in `docs/` (Quarto creates this automatically)

## Dependencies

- R (>= 4.0)
- Quarto
- R packages: tidyverse, ranger, caret, viridis, ggplot2, scales

