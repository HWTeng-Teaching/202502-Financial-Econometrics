## Q.04

![image](https://github.com/user-attachments/assets/e7f650d6-638c-4560-84dd-4bbdd6e2e19c)

## Ans:
**(a)**
Model 1: 擬合值計算

模型 1 的方程式為：RATING = 64.289 + 0.990 × EXPER

![image](https://github.com/user-attachments/assets/bcbe5d55-fc54-4455-ab90-385821e38d16)

![image](https://github.com/user-attachments/assets/5fb7930d-60e1-4119-b261-3e6ed200f87b)

Model 1（線性回歸） 顯示評分隨經驗線性增加。

--------------------------------------------------------------

**(b)**
模型 2 的方程式為：

RATING = 39.464 + 15.312 ln(EXPER)

![image](https://github.com/user-attachments/assets/fa5a35a0-e9d4-446a-979d-aa56e3b3a00b)

![image](https://github.com/user-attachments/assets/d4879e47-6838-43ce-a05b-555ca43d3542)

Model 2（對數回歸） 顯示早期經驗的影響較大，但隨著時間推移邊際效益遞減。

--------------------------------------------------------------

**(c)**
Model 1: 邊際效應

模型 1 為線性模型，邊際效應為固定值：d(RATING) / d(EXPER) = 0.990

Model 1（線性回歸）：邊際影響為 0.990，固定不變，表示無論經驗多少，每增加一年經驗，評分都會提高 0.990。因此，無論藝術家有多少經驗，額外一年的影響皆為 0.990。

![image](https://github.com/user-attachments/assets/1a2fd9e9-8b3d-4dfe-8b88-1fbfa7403edd)

--------------------------------------------------------------

**(d)**
模型 2 的邊際效應計算如下：d(RATING) / d(EXPER) = 15.312 / EXPER

![image](https://github.com/user-attachments/assets/61f2ebca-1462-49ea-99d4-900e5eacfba2)

Model 2（對數回歸）：

10 年經驗時，邊際影響為 1.5312，表示經驗增加一年，評分提高較多。

20 年經驗時，邊際影響下降至 0.7656，顯示邊際效益遞減，符合學習曲線理論。

這表明當經驗值增加時，額外一年經驗的影響會逐漸減少。

![image](https://github.com/user-attachments/assets/93feffbb-05f8-4c8a-8454-f4d7a1b72b6e)

![image](https://github.com/user-attachments/assets/5b94a26c-6b9d-4904-897c-512a26189f87)

--------------------------------------------------------------

**(e)**

![image](https://github.com/user-attachments/assets/80f5d53a-5b70-47c7-9cbf-1f983b71cfb1)

根據 𝑅2值比較：

Model 1（所有數據）： 𝑅2=0.3793（擬合效果較差）。

Model 1（僅有經驗者）：𝑅2=0.4858（稍有改善，但仍不如 Model 2）。

Model 2（對數回歸）：𝑅2=0.6414（擬合效果最佳）。

--------------------------------------------------------------

**(f)**

Model 1 假設每年經驗對評分的影響固定，適用於經驗少的個體，但不能解釋隨時間變長後的邊際變化，這在現實中不太合理。

Model 2 使用對數形式，擬合數據較好，因為它解釋了 64.14% 的變異，顯示邊際效應遞減，這與學習曲線概念相符。

因此，Model 2比 Model 1 適合度更高，更符合經濟學概念（學習曲線）。









