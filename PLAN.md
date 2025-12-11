# Plan: Document Restructuring and Visualization Updates

## Overview
This plan outlines changes to restructure code chunks, add confusion matrices, and modify visualizations across multiple Quarto documents.

## 1. Code Chunk Restructuring in category.qmd and family.qmd

### 1.1 category.qmd Changes

**Location**: After "Save Model" section (after line 240)

**New Section Structure**:
- **Random Forest Model Summary**
  - Code chunk with model object reference and brief description
  - Filler text: "The random forest model for category prediction demonstrates [performance characteristics]. The model utilizes [number] features to distinguish between malware categories."
  
- **SHAP Values Analysis**
  - Code chunk displaying SHAP values for selected instances
  - Filler text: "SHAP (SHapley Additive exPlanations) values provide insight into feature contributions for individual predictions. The analysis reveals which features drive classification decisions for each category instance."
  
- **LIME Explanations**
  - Code chunk displaying LIME explanations
  - Filler text: "LIME (Local Interpretable Model-agnostic Explanations) generates local approximations of the model's behavior. These explanations highlight the most influential features for each prediction."

**Confusion Matrix Addition**:
- Add a new section "Confusion Matrix Visualization" after "Evaluate Model"
- Create a formatted confusion matrix plot using ggplot2
- Display as a heatmap with counts and percentages

### 1.2 family.qmd Changes

**Location**: After "Save Models" section (after line 274)

**New Section Structure** (for each category: Adware, Riskware, Trojan):
- **{Category} Random Forest Model Summary**
  - Code chunk with model object reference
  - Filler text: "The {Category} family prediction model classifies malware families within the {Category} category. The model achieves [accuracy] accuracy in distinguishing between different family variants."
  
- **{Category} SHAP Values Analysis**
  - Code chunk displaying SHAP values for the corresponding instance
  - Filler text: "SHAP values for the {Category} instance reveal the feature contributions specific to family-level classification. Key distinguishing features include [feature categories]."
  
- **{Category} LIME Explanations**
  - Code chunk displaying LIME explanations
  - Filler text: "LIME explanations for the {Category} family model show localized feature importance patterns that differ from category-level predictions."

**Confusion Matrix Addition**:
- Add confusion matrix visualization for each category model
- Create separate sections: "Adware Family Model Confusion Matrix", "Riskware Family Model Confusion Matrix", "Trojan Family Model Confusion Matrix"
- Display as formatted heatmaps

## 2. Confusion Matrix Visualizations

### 2.1 category.qmd
- **Location**: After "Evaluate Model" section, before "Confusion Matrix" text output
- **Implementation**: 
  - Create ggplot2 heatmap of confusion matrix
  - Use color scale to show prediction accuracy
  - Include counts and percentages in cells
  - Format: Predicted (rows) vs Actual (columns)

### 2.2 family.qmd
- **Location**: After each category model evaluation in the training loop
- **Implementation**:
  - Create confusion matrix visualizations for Adware, Riskware, and Trojan models
  - Store confusion matrices during training
  - Display in "Model Results" section with formatted plots
  - Each plot should show family-level predictions

## 3. Database Read vs Write Activity Heatmap

### 3.1 Single Heatmap for index.qmd
- **Source**: Currently in proposal.qmd (lines 578-596) as `p_db_read_write`
- **Changes**:
  - Replace `geom_point()` with `geom_density_2d()`
  - Change color palette to `scale_fill_viridis_c(option = "inferno")`
  - Remove `colour = Category` mapping, use density contours instead
  - Keep same data: `get_combined_data()` with `log_DB_reads` and `log_DB_writes`
- **Location**: Add new code chunk in index.qmd after "Project Overview" section
- **Filler text**: "Database activity patterns reveal distinct read-write relationships across malware categories. The density heatmap highlights regions of concentrated activity."

### 3.2 Faceted Heatmaps
- **Source**: Currently in proposal.qmd (lines 599-624) as `p_db_read_write_fam`
- **Changes**:
  - Replace `geom_point()` with `geom_density_2d()`
  - Remove `colour = fam_color` mapping
  - Remove `scale_colour_identity()` and legend
  - Use `scale_fill_viridis_c(option = "inferno")` for density
  - Keep `facet_wrap(~ Category, scales = "free")`
  - Update subtitle to remove mention of "Top 4 families"
- **Location**: Keep in proposal.qmd, update existing code chunk

## 4. Dynamic Dex Loading Plot Migration

### 4.1 Move to index.qmd
- **Source**: Currently in proposal.qmd (lines 640-662) as `p_dex_bar`
- **Changes**:
  - Change `category_fill` to `scale_fill_manual(values = rep("red", length(unique(dex_summary$Category))))`
  - Remove legend: add `theme(legend.position = "none")`
  - Update theme to remove category-based coloring
- **Location**: Add new code chunk in index.qmd, after Database heatmap chunk
- **Filler text**: "Dynamic code loading through DexClassLoader APIs varies significantly across malware categories, with some categories showing higher adoption rates than others."

## Implementation Details

### Code Chunk Organization
- All new code chunks should have appropriate labels (e.g., `#| label: category-model-summary`)
- Use `cache: true` for data loading/preparation chunks
- Use `cache: false` for model training and explanation chunks
- Add `echo: false` for display-only chunks if needed

### Filler Text Guidelines
- Provide context about what the code does
- Explain the purpose of SHAP/LIME values
- Describe model performance characteristics
- Avoid technical jargon where possible
- Do NOT include: `glimpse()`, `str()`, `head()`, or similar data inspection functions

### Visualization Specifications
- **Heatmaps**: Use `geom_density_2d()` with `stat = "density_2d"` and `geom = "polygon"`
- **Color Palette**: `scale_fill_viridis_c(option = "inferno")` for density plots
- **Confusion Matrices**: Use `geom_tile()` with appropriate color scales
- **Consistent theming**: Maintain `andmal_theme` where applicable

## Files to Modify

1. `ProjectProposal/category.qmd`
   - Add model summary section with filler text
   - Add SHAP values code chunk with filler text
   - Add LIME explanations code chunk with filler text
   - Add confusion matrix visualization

2. `ProjectProposal/family.qmd`
   - Add model summary sections for each category (3 sections)
   - Add SHAP values code chunks for each category (3 chunks)
   - Add LIME explanations code chunks for each category (3 chunks)
   - Add confusion matrix visualizations for each category model (3 visualizations)

3. `ProjectProposal/index.qmd`
   - Add Database read vs write heatmap code chunk
   - Add Dynamic Dex loading bar chart code chunk (moved from proposal.qmd)

4. `ProjectProposal/proposal.qmd`
   - Modify `p_db_read_write` to use `geom_density_2d()` with inferno palette
   - Modify `p_db_read_write_fam` to use `geom_density_2d()` with inferno palette, remove family coloring
   - Remove `p_dex_bar` code chunk (moving to index.qmd)

## Dependencies

- `ggplot2` - for all visualizations
- `viridis` - for inferno color palette
- `lime` - for LIME explanations
- `fastshap` - for SHAP values
- `ranger` - for random forest models
- `caret` - for confusion matrix creation

