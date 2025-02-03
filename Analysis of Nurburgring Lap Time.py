import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.api as sm
from scipy import stats
import numpy as np
from statsmodels.stats.outliers_influence import variance_inflation_factor

file_path = "C:\\Users\\卓乔\\Downloads\\LP_MNAR.xlsx"
lap_time = pd.read_excel(file_path, sheet_name='Sheet1')
ltm = lap_time['Lap Time Minutes']
lts = lap_time['Lap Time Seconds']
hp = lap_time['Horse Power ']
tq = lap_time['Torque']
kw = lap_time['Kerb Weight']
ec = lap_time['Effective Capacity']
cyl = lap_time['Cylinders']
acc = lap_time['Acceleration']
mpg = lap_time['Gas Mileage']
co2 = lap_time['CO2 Emmission']
tp = lap_time['Maximum Speed']
tm = lap_time['Transmission ']
layout = lap_time['Layout']
ep = lap_time['Engine Position']
lt = ltm*60 + lts

plt.boxplot([lt, hp, tq, cyl, acc, mpg, co2, tp],tick_labels=['Lap Time', 'Horsepower','Torque', 'Cylinders', 'Acceleration', 'Gas Mileage', 'CO2 Emmission', 'Maximum Speed'])
plt.title("Boxplot of Selected Predictors")
plt.xlabel("Category")
plt.ylabel("Value")
#plt.show()

plt.boxplot([kw,ec],label=['Kerb Weight', 'Effective Capacity'])
plt.title("Boxplot of Kerb Weight and Effective Capacity")
plt.xlabel("Category")
plt.ylabel("Value")
#plt.show()

def fitted_residuals_plot(data):
    x = data
    y = lt
    x = sm.add_constant(x)
    model = sm.OLS(y, x).fit()
    residuals = model.resid
    fitted = model.fittedvalues
    plt.scatter(fitted, residuals,edgecolors='k',facecolors='none')
    plt.axhline(y=0, color='r', linestyle='--',linewidth=1)
    plt.xlabel('Fitted Values')
    plt.ylabel('Residuals')
    plt.title('Fitted Values vs. Residuals')
    plt.show()

def normality_check(data):
    plt.scatter(data,lt)
    statistic, p_value = stats.shapiro(data)
    plt.text(0.05, 0.95, f"Shapiro-Wilk p-value: {p_value:.3f}",
         transform=plt.gca().transAxes,
         fontsize=12, verticalalignment='top', bbox=dict(facecolor='white', alpha=0.5))
    plt.show()
    
trans_ec = [n**(-1.5) for n in ec]
trans_tq = [n**(-2) for n in tq]
trans_co2 = [n**(-1.5) for n in co2]
trans_cyl = [n**(-1) for n in cyl]
trans_kw = [n**(-3) for n in kw]    
#predictors = [hp,trans_tq,trans_kw,trans_ec,trans_cyl,acc,mpg,trans_co2,tp,tm,layout,ep]

corrmatrixdata = {
    'lt':lt,
    "hp": hp,
    "trans_tq": trans_tq,
    "trans_kw": trans_kw,
    "trans_ec": trans_ec,
    "trans_cyl": trans_cyl,
    "acc": acc,
    "mpg": mpg,
    "trans_co2": trans_co2,
    "tp": tp,
}
corrmatrixdf = pd.DataFrame(corrmatrixdata)
corr_matrix= corrmatrixdf.corr()

vifdata = {
    "hp": hp,
    "trans_tq": trans_tq,
    "trans_kw": trans_kw,
    "trans_ec": trans_ec,
    "trans_cyl": trans_cyl,
    "acc": acc,
    "mpg": mpg,
    "trans_co2": trans_co2,
    "tp": tp,
    'tm': tm,
    'layout': layout,
    'ep': ep
}
vifdf = pd.DataFrame(vifdata)
vifdf_dummies  = pd.get_dummies(vifdf)
vifdf_dummies = vifdf_dummies.astype(float)
vifdf_dummies = vifdf_dummies.apply(pd.to_numeric, errors='coerce').dropna()
vif_data = pd.DataFrame()
vif_data['feature'] = vifdf_dummies.columns
vif_data["VIF"] = [variance_inflation_factor(vifdf_dummies.values, i) for i in range(vifdf_dummies.shape[1])]

combined_var = [sum(val) for val in zip(hp, trans_tq,trans_ec,trans_cyl,acc,trans_co2)]

predictors  = {
    'combined var': combined_var,
    'mpg': mpg,
    'tp': tp,
    'tm':tm,
    'layout': layout,
    'ep': ep,
    'lt':lt   
}
df = pd.DataFrame(predictors)
df = df.apply(pd.to_numeric, errors='coerce')
numeric_cols = ['combined var', 'mpg', 'tp', 'lt']
df[numeric_cols] = df[numeric_cols].apply(pd.to_numeric, errors='coerce')
df = df.dropna(subset=numeric_cols) 
categorical_cols = ['tm', 'layout', 'ep']
df_encoded = pd.get_dummies(df, columns=categorical_cols).dropna()
X = df_encoded.drop('lt', axis=1)
y = df_encoded['lt']
X = sm.add_constant(X)
Nurburgring_model = sm.OLS(y, X).fit()
print(Nurburgring_model.summary())