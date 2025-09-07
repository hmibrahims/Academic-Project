
---

## ⚙️ Workflow  

1. **Preprocessing**  
   - Convert DN → TOA reflectance & radiance  
   - Compute NDVI (vegetation) & NDBI (built-up index)  
   - Estimate LST from thermal bands  
   - Clip all rasters to AOI  

2. **UHI Analysis**  
   - Define **urban vs rural masks** from NDVI & NDBI thresholds  
   - Calculate mean LST for both classes  
   - Compute SUHI intensity = Urban – Rural (°C)  

3. **Outputs**  
   - Continuous **SUHI maps** (`*_SUHI_map.tif`)  
   - Classified **SUHI rasters** (5 classes: ≤0, 0–2, 2–4, 4–6, >6 °C)  
   - CSV summaries (`SUHI_summary.csv`, class areas long/wide)  
   - Visualizations (time series, stacked bars, histograms, donuts)  

---

## 📊 Example Outputs  

- **SUHI map (continuous):** Spatial distribution of urban–rural anomaly in °C  
- **SUHI classes (discrete):** 5-category heat intensity map  
- **Time series plot:** SUHI intensity over 2013–2025  
- **Stacked bar chart:** % AOI area by SUHI class per year  
- **Urban vs Rural LST plot:** Trends of mean LST for both zones  

---

## 🛠 Tools & Libraries  

- **R**  
  - terra  
  - dplyr  
  - tidyr  
  - ggplot2  
  - readr  
- **QGIS/ArcGIS** for map visualization  
- **Landsat 8/9** (USGS Earth Explorer)  

---

## 📑 Report Integration  

The outputs feed directly into:  
- **Figures:** SUHI maps, histograms, time series, stacked class trends  
- **Tables:** Mean LST (urban vs rural), % area per SUHI class  
- **Discussion:** Spatial & temporal UHI trends, policy recommendations  

---

## 🚀 Getting Started  

1. Clone this repo:  
   ```bash
   git clone https://github.com/<your-username>/UHI-Landsat-Analysis.git
   cd UHI-Landsat-Analysis
