# **Oncology Dashboard — Interactive Clinical Review Application**

## **📌 Purpose of the Application**

The app is designed to simplify and accelerate clinical data review in oncology studies.  
It brings together analysis datasets, statistical outputs, and visual summaries into a single interface, 
allowing reviewers to:

- Validate whether statistical graphs and tables accurately reflect underlying ADaM datasets  
- Explore patient‑level trends dynamically  
- Reduce manual cross‑checking between multiple static outputs  
- Improve communication between statisticians, programmers, and clinicians  

This dashboard transforms the traditional review workflow into a faster, more transparent, 
and more intuitive experience.

---

# **📂 Key Modules in the Dashboard**

## **1. Tumor Growth (Spider Plot)**
This module visualizes **Change From Baseline** across visits for each subject.

- **`read_excel("Tumor Growth")`**  
  Loads tumor growth data directly from the ADaM‑style sheet.
- **Reactive filtering by TNF status**  
  Allows reviewers to compare tumor trajectories between TNF‑positive and TNF‑negative subjects.
- **Spider plot (geom_line + geom_point)**  
  Shows individual subject tumor changes over time, helping identify responders vs non‑responders.
- **Summary statistics table**  
  Provides mean, median, and SD per visit — useful for quick trend assessment.

This tab helps reviewers quickly understand tumor burden evolution and verify consistency with response assessments.

---

## **2. Kaplan–Meier Survival Analysis**
This module displays KM curves stratified by TNF status.

- **`survfit(Surv(Time, Event) ~ TNF_Status)`**  
  Fits survival curves based on ADTTE‑like data.
- **`ggsurvplot()`**  
  Provides risk tables, confidence intervals, and p‑values — essential for clinical interpretation.
- **TNF filtering**  
  Enables subgroup survival comparisons.

This tab supports validation of survival endpoints and ensures alignment between dataset values and plotted KM curves.

---

## **3. Swimmer Plot (Overall Response + Duration of Response)**
This module integrates **Overall Survival (OS)**, **Best Overall Response (BOR)**, and **Duration of Response (DOR)**.


#### **Data Preparation**
- **ADSL + ADTTE + ADRS merges**  
  Combine subject‑level data, time‑to‑event endpoints, and response assessments.
- **Derivation of PR/PD dates**  
  Extracts first PR/CR date and earliest PD date using grouped logic:  
  *“first_pr_date = min(ADT where AVALC ∈ {CR, PR})”*  
  *“pd_date = min(ADT where AVALC == PD)”*  
- **Duration calculations (`start`, `end`)**  
  Converts dates into numeric durations relative to RFSTDT.

These derivations allow accurate representation of DOR bars overlaid on OS bars.

#### **Plotting**
- **Swimmer bars (`geom_segment`)**  
  Show OS duration per subject.
- **DOR overlay (optional)**  
  Highlights response duration within the OS timeline.
- **BOR labels**  
  Provide immediate visual classification of response category.
- **Censoring/Death markers**  
  Shapes and colors differentiate censored vs death events.

This tab gives reviewers a consolidated view of treatment effect, response durability, and survival outcome — all in one graphic.

---

# **🔧 Summary of Data Manipulations**

Below is a concise description of the major transformations performed in the app:

### **1. Tumor Growth**
- Filter by TNF status  
- Compute summary statistics (mean, median, SD) per visit  
- Exclude baseline from summary calculations  

### **2. Kaplan–Meier**
- Filter KM dataset by TNF status  
- Fit survival curves using `Time` and `Event` variables  
- Generate risk tables and p‑values for subgroup comparison  

### **3. Response & DOR Derivations**
From the ADRS sheet:

- Convert ADT to proper date format  
- Identify earliest PR/CR date  
- Identify earliest PD date  
- Determine last visit date  
- Merge with ADSL to obtain RFSTDT  
- Compute numeric durations:  
  - **start = first_pr_date − RFSTDT**  
  - **end = PD date or last visit − RFSTDT**

### **4. OS Dataset Preparation**
- Filter ADTTE for OS records  
- Merge BOR from ADRS  
- Merge DOR dataset  
- Derive censoring status labels  
- Compute end date for swimmer bars  

These manipulations ensure that statistical outputs (KM curves, swimmer plots) accurately reflect underlying ADaM‑like datasets.

---

# **🚀 How the Dashboard Improves Review Efficiency**

### **✔ Single integrated platform**
No need to open multiple listings, tables, or figures — everything is interactive and connected.

### **✔ Faster validation**
Reviewers can instantly verify whether plotted values match dataset values.

### **✔ Clear visualization of clinical endpoints**
Tumor growth, survival, response, and DOR are all represented visually.

### **✔ Transparent derivations**
The app shows how raw ADaM variables transform into analysis‑ready endpoints.

### **✔ Enhanced communication**
Statisticians, programmers, and clinicians can review the same interactive outputs, reducing misunderstandings.
