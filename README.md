# EpiCardio
**Author:** Jose Alejandro Avila Cabreja

*EpiCardio* is a shiny app built using the R language, which allows you to visualise trends in cardiovascular disease mortality in Cuba between 2010 and 2020. Data are categorised by age, sex, specific cause and province. This is a purely informative tool and is not intended to replace the official means of dissemination of the Ministry of Public Health.

## Data source
The data were extracted from health statistical yearbooks published between 2011 and 2021. These can be consulted on the <a href='https://files.sld.cu/bvscuba/category/anuario-estadistico/'>Virtual Health Library</a> web page.

## Methodology
All information was extracted directly from health statistical yearbooks and entered into a database created in MS Excel for further management and validation.<br><br>The crude mortality rate (CMR) was calculated using the following formula:

$$CRM = \dfrac{TD}{TP} *100 000$$

*where:*
 * TD: Total deaths
 * TP: Total population
<br>

The annualised rate of change (ARC) was calculated using the following formula:

$$ARC = \dfrac{log(MRs) - log(MRe)}{I} *100$$

<i>where:</i>
<ul>
            <li>MRs: Mortality rate at the start of the time interval</li>
            <li>MRe: Mortality rate at the end of the time interval</li>
            <li>I: Number of year in the interval</li>
        </ul>
<br>
The 95 % confidence interval was calculated for all estimates.<br><br>Predictions for the next 5 years were calculated using a Elastic-Net Regularized Generalized Linear Model.
