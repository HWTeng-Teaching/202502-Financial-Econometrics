***學號：313707017     資財所碩一 : 林子恩***


![螢幕擷取畫面 2025-04-21 002813](https://github.com/user-attachments/assets/b6a785c0-86f3-4c69-b77a-dc2868144693)

d.

![image](https://github.com/user-attachments/assets/fe211618-1511-4991-ad30-8b8a32f0b6d8)
![image](https://github.com/user-attachments/assets/afed8c5a-f364-40f6-b428-7df779b00a41)
![image](https://github.com/user-attachments/assets/1bfb115b-3aa8-4980-b87a-9ea3d90823ba)


Traditional OLS standard errors assume constant error variance (homoskedasticity). If this assumption is violated—for example, if wage variance changes with metro or experience—OLS standard errors can be biased, often underestimated.
White's robust standard errors correct for heteroskedasticity by allowing error variance to vary. As a result, robust standard errors are usually larger, leading to wider confidence intervals.
Wider intervals suggest that traditional standard errors may be overly optimistic, potentially causing overconfidence in the results.

e.

![image](https://github.com/user-attachments/assets/17a23782-3911-4e4f-bf11-3427e8f20773)

OLS (White robust) does not assume an error variance structure and directly adjusts standard errors to address heteroskedasticity, making results more robust but potentially less efficient (wider confidence intervals).

FGLS assumes error variance can be modeled using metro and exper. If the assumption is correct, FGLS is more efficient (narrower confidence intervals); if incorrect, FGLS may perform poorly.

f.

![image](https://github.com/user-attachments/assets/d83d6330-fe68-49c0-b4c7-0c1ee88b3763)

For part (d), OLS (White robust) does not assume any variance structure and directly adjusts standard errors to handle heteroskedasticity, typically resulting in wider but more reliable confidence intervals.

FGLS (robust standard errors) may have narrower intervals than OLS (White robust) if the heteroskedasticity model is partially correct, but wider intervals than FGLS in part (e) since robust standard errors account for model errors.

g.

Choosing FGLS (White robust):

FGLS (White robust) has slightly smaller standard errors than OLS (White robust) (e.g., for educ: 0.0074135815 vs. 0.0074670055), leading to narrower confidence intervals and slightly higher efficiency.

Additionally, using White robust standard errors ensures that even if the heteroskedasticity model (metro and exper) is misspecified, the standard errors and confidence intervals remain reliable, avoiding the risk of FGLS (traditional standard errors) underestimating uncertainty.
