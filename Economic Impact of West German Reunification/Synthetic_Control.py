import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit

#–––––––––––––––GET PARAMETERS–––––––––––––––#

def nl_model(x, a, b, c, d, e):
    denom = (1 / (1 + np.exp(a) + np.exp(b) + np.exp(c) + np.exp(d) + np.exp(e)))
    return (1 / (1 + np.exp(a) + np.exp(b) + np.exp(c) + np.exp(d) + np.exp(e))) * x[0] + \
        (np.exp(a) / denom) * x[1] + \
        (np.exp(b) / denom) * x[2] + \
        (np.exp(c) / denom) * x[3] + \
        (np.exp(d) / denom) * x[4]

#–––––––––––––––––CALCULATE WEIGHTS–––––––––––––––#
def calculate_weights(params):
    """
    Given a list of at least five parameters, return a list with associated weights
    Parameters:
        params: list of numbers
    """
    weights = [np.exp(params[i])/ (1 + np.exp(params[0]) + np.exp(params[1]) + np.exp(params[2]) 
                                   + np.exp(params[3]) + np.exp(params[4])) for i in range(len(params) - 1)]
    first = [(1 / (1 + np.exp(params[0]) + np.exp(params[1]) + np.exp(params[2]) 
                   + np.exp(params[3]) + np.exp(params[4])))]
    return first + weights

#––––––––––––––––––––––PLOTS––––––––––––––––––––––#
def show_gdp_plots(wg, synth_wg, years):
    """
    Plots both West Germany's and Synthetic West Germany's GDP
    Per Capita from 1960-2003
    Params: 
        wg: GDP per capita of West Germany
        synth_wg : GDP per capita of synthetic West Germany
        years: list of years data was collected
    """
    plt.plot(years, wg, label='Real West Germany', color='black', linestyle='-')
    plt.plot(years, synth_wg, label='Synthetic West Germany', color='blue', linestyle='--')
    plt.xlabel('Year')
    plt.ylabel('Per Capita GDP (PPP, 2002 USD)')
    plt.title('Trends in Per Capita GDP: West Germany versus Synthetic West Germany')
    plt.axvline(x = 1990, color = 'red', linestyle = 'dotted')
    plt.legend()
    plt.show()
    print(wg, synth_wg, wg - synth_wg)

def show_diff_plot(wg, synth_wg, years):
    """
    Plots the gap in GDP Per Capita between West Germany and Synthetic West 
    Germany from 1960-2003
    Params: 
        wg: GDP per capita of West Germany
        synth_wg : GDP per capita of synthetic West Germany
        years: list of years data was collected
    """
    delta = wg - synth_wg
    plt.plot(years, delta, color='black', linestyle='-')
    plt.xlabel('Year')
    plt.ylabel('Gap in Per capita GDP (PPP, 2002 USD)')
    plt.title('Per Capita GDP Gap between West Germany and Synthetic West Germany')
    plt.axvline(x = 1990, color = 'red', linestyle = 'dotted')
    plt.axhline(y = 0, color = 'red', linestyle = 'dotted')
    plt.show()

def main():
    #–––––––––––––––––FORMAT DATA–––––––––––––––#
    # Read data
    data = pd.read_csv("/Users/mjdelg/Desktop/ECON 303/germany.csv")
    data = data.pivot(index = "Country Name", columns = "Year", values= "GDP per-capita (annual)")

    # Get list of years of available data
    years = list(data.columns)
    data = data.T

    # Get gdp for each country before reunification
    aus_0, jap_0, neth_0, switz_0, usa_0, wg_0 = data.iloc[:30, 0], data.iloc[:30, 1], data.iloc[:30, 2], data.iloc[:30, 3], data.iloc[:30, 4], data.iloc[:30, 5]
    # Get gdp for each country
    aus, jap, neth, switz, usa, wg = data.iloc[:, 0], data.iloc[:, 1], data.iloc[:, 2], data.iloc[:, 3], data.iloc[:, 4], data.iloc[:, 5]

    x_data = np.array([aus_0, jap_0, neth_0, switz_0, usa_0])

    # Use non-linear model 
    params, covar = curve_fit(nl_model, x_data, wg_0)

    # Calculate weights to see which countries West Germany was most like pre-reunification
    w1,w2,w3,w4,w5 = calculate_weights(params)

    # Check that weights make sense
    print(sum(calculate_weights(params)))

    # Get GDP per capita values for synthetic West Germany
    synth_wg = w1*aus + w2*jap + w3*neth + w4*switz+ w5*usa

    # Plot difference in GDP
    show_gdp_plots(wg, synth_wg, years)

    # Plot gap in per capita GDP
    show_diff_plot(wg, synth_wg, years)

if __name__ == "__main__":
    main()

