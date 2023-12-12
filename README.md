# Car-Insurance-Claim-Prediction
The "Car Insurance Claim Prediction" aims to predict if policyholders will file an insurance claim in the next six months by analyzing a comprehensive dataset. This insight will help insurance companies refine their risk assessment and pricing strategies. 

## Project Motivation
The "Car Insurance Claim Prediction" project is driven by the evolving landscape of data analytics and predictive modeling in the insurance sector, particularly car insurance. Analyzing a comprehensive dataset, the project aims to predict policyholders' likelihood of filing a claim within six months. This predictive insight will revolutionize risk assessment and pricing strategies, empowering insurance companies to manage risk in the industry proactively.
The primary objective is to craft a highly accurate predictive model using policyholder attributes. This model will facilitate data-informed decision-making, allowing insurance firms to pre-emptively address risks and optimize operational efficiency.

## Data Description 
Data mining techniques have a wide range of applications across various domains. For our project, we have chosen to work on the problem of predicting car insurance claims. We have collected 43 input attributes such as policy tenure, age of the car, model, segment, fuel type, etc. The target variable is whether a claim is made, represented by the binary variable 'is_claim.’ This is a classification problem, and we have used four different algorithms - Logistic Regression, Decision Trees, Random Forests, and Neural Networks - to predict the loan status.
The dataset we have taken is from Kaggle, and it has over 43 input attributes that help us understand the Claim prediction. We have plotted several graphs as part of our exploratory data analysis to understand our data better.

## Exploratory Data Analysis
During the exploratory data analysis (EDA) phase, we thoroughly examined the dataset, paying close attention to different aspects. We aimed to extract valuable insights from the data and ensure it was well-prepared for modeling purposes.  
**[1]**  Verifying the summary of the dataset structure to understand the data values.

**[2]**  Next, we verify that we have any NA or NULL values in the dataset.  

**[3]**  A pie chart is generated by the code to display the distribution of instances belonging to the 'No Claim' and 'Claim' categories. This pie chart helps to reveal the class imbalance in the target variable 'is_claim.' The legend and colors used in the chart accurately depict the two categories, emphasizing the necessity of addressing this imbalance while constructing the model to achieve better predictive accuracy.
The chart shows the percentage of No claims for car insurance and claims for car insurance.  
**•	The Percentage of No claims for car insurance is 93.4%**  
**•	The percentage of claims for car insurance is 6.4%.**  
![Pie chart](https://github.com/krishnaapurva/Car-Insurance-Claim-Prediction/assets/41700695/d8445aca-5817-4b73-8650-9a059e492481)


**[4]** The count plots showcasing the distribution of different categorical variables like 'area_cluster,' 'segment,' 'model,' 'fuel_type,' 'max_torque,' 'max_power,' and 'engine_type' concerning the 'No Claim' and 'Claim' classes. Each plot visualizes the count of occurrences for both classes, aiding in understanding the class distribution within these categorical features. These visualizations offer insights into potential correlations between categorical variables and the claim outcomes, providing a clear understanding of their impact on the target variable.  

![area_cluster](https://github.com/krishnaapurva/Car-Insurance-Claim-Prediction/assets/41700695/e3d5445a-38e9-4d40-baaa-bf526866c8da)

![segment](https://github.com/krishnaapurva/Car-Insurance-Claim-Prediction/assets/41700695/7836e176-963c-42c3-a6be-4a086ebfcfc5)

![model](https://github.com/krishnaapurva/Car-Insurance-Claim-Prediction/assets/41700695/da0c94f5-0d2e-4573-a6db-822523664a29)

![fuel_type](https://github.com/krishnaapurva/Car-Insurance-Claim-Prediction/assets/41700695/9a090d82-452e-4ecc-b947-180dd8ce7cd9)

![max_torque](https://github.com/krishnaapurva/Car-Insurance-Claim-Prediction/assets/41700695/5d28f828-98b2-4ac1-b8ba-c09b016024a5)

![max_power](https://github.com/krishnaapurva/Car-Insurance-Claim-Prediction/assets/41700695/0fcd079a-8d89-4a82-8d45-d9d2a5bc839f)

![engine_type](https://github.com/krishnaapurva/Car-Insurance-Claim-Prediction/assets/41700695/101152ce-d3c7-4b91-9130-2a7a18e357f1)


**[5]** As part of our data processing, we extract numerical values in Newton meters (Nm) and revolutions per minute (rpm) from the 'max_torque' data. After that, we calculate the ratio of torque to rpm and add a new column named 'torque_to_rpm_ratio' to the dataset to consolidate this information. Similarly, we are doing this for the ‘max_power’ data.  

**[6]** To handle binary categorical columns in the dataset 'df.train_filter', we first identified the columns prefixed with "is_" using a pattern-matching approach. After the identification process, we proceeded to convert the categorical values ("Yes" and "No") into numeric format by assigning 1 to "Yes" and 0 to "No" across all the identified 'is_' columns within the dataset.  

**[7]** The variable 'columns_to_convert' holds the names of all the 'is_' columns that require conversion from categorical binary values to numeric representations. This conversion ensures consistency in the dataset 'df.train_filter' format for any further analytical or modeling purposes.  

**[8]** We use the 'fastDummies' library to convert categorical data into a numerical format by generating dummy variables. We remove original categorical columns and exclude specific extra dummy variables to avoid overfitting and ensure a streamlined and effective dataset for analysis and modeling.  

**[9]** We standardize column names by replacing spaces, periods, and hyphens with underscores.  

**[10]** To train and evaluate our predictive model, we partitioned the dataset randomly into two subsets: training and validation. The training set comprised **60%** of the data, while the validation set contained **40%**. This approach ensured that the model was trained on a significant portion of the data while also allowing us to evaluate its performance on a separate subset.  

**[11]** To resolve the class imbalance issue in the training dataset, we use an oversampling technique that involves replicating instances of the minority class (where 'is_claim' equals 1) until the count of both classes is equal. This oversampling is performed through the ovun.sample() function available in the 'ROSE' package. Following this, we calculate the number of instances where 'is_claim' equals 1 in the balanced dataset. This helps in achieving a more balanced distribution of classes within the training data.  

**[12]** Presenting the distribution of claims and no-claims after performing oversampling.  

![after_oversampling](https://github.com/krishnaapurva/Car-Insurance-Claim-Prediction/assets/41700695/3339869d-a3ce-4741-b84e-4a44c845b707)

## Random Forest – 
**[1] MODEL 1 – 10 TREES**  

The Random Forest model was trained to predict the 'is_claim' variable, and three different iterations were employed with varying parameters. The initial configuration had 10 trees, resulting in an accuracy of around **61.28%**. The confusion matrix showed that the model had moderate performance in correctly identifying 'No Claim' instances while struggling with 'Claim' predictions. The sensitivity was **61.62%**, and the specificity was **56.43%**, with an Area Under the Curve (AUC) value of **0.621**.

**CONFUSION MATRIX FOR RANDOM FOREST (10 TREES) –**  

<img width="468" alt="image" src="https://github.com/krishnaapurva/Car-Insurance-Claim-Prediction/assets/41700695/89243e62-73f0-4814-9e44-10ac8e512f96">

**ROC  CURVE FOR RANDOM FOREST (10 TREES) –**  

<img width="468" alt="image" src="https://github.com/krishnaapurva/Car-Insurance-Claim-Prediction/assets/41700695/2e1a5846-4dcd-4b56-b465-2d88b8075df4">

**AREA UNDER THE CURVE: 0.621**

**[2] MODEL 2 – 100 TREES**
The model's tree count was increased to 100 in the subsequent iteration, aiming for improved predictive accuracy. Although the accuracy was slightly improved to around 61.75%, the model's performance metrics remained relatively consistent with the previous iteration. The sensitivity was recorded at 62.06% and specificity at 57.16%, with a marginally increased AUC of 0.6316.

**CONFUSION MATRIX FOR RANDOM FOREST (100 TREES) –**

<img width="339" alt="image" src="https://github.com/krishnaapurva/Car-Insurance-Claim-Prediction/assets/41700695/3049312d-cb58-44de-bbc5-83f9f5d6b8c7">

**ROC CURVE FOR RANDOM FOREST (100 TREES) –**  

<img width="468" alt="image" src="https://github.com/krishnaapurva/Car-Insurance-Claim-Prediction/assets/41700695/1513a8f9-6c94-448f-a254-1814feb230c3">

**AREA UNDER THE CURVE: 0.6316**

**[3] MODEL 3 - 500 TREES**
Further refinement was attempted by increasing the number of trees to 500, with the model displaying similar performance trends. The accuracy stabilized around 61.61%, with a sensitivity of 61.87% and a specificity of 57.89%. The AUC marginally increased to 0.6317.  


**CONFUSION MATRIX FOR RANDOM FOREST (500 TREES) –** 

<img width="389" alt="image" src="https://github.com/krishnaapurva/Car-Insurance-Claim-Prediction/assets/41700695/cd494c3e-8ffa-47f6-8b99-58c171bad7fa">

**ROC CURVE FOR RANDOM FOREST (500 TREES)  –** 

<img width="468" alt="image" src="https://github.com/krishnaapurva/Car-Insurance-Claim-Prediction/assets/41700695/19f7f07a-c59b-43a6-b6d4-4bdaa2b48cf7">

**AREA UNDER THE CURVE: 0.6317**  

In conclusion, despite multiple iterations with varying tree counts, the Random Forest model's predictive performance remained relatively consistent. The model exhibited a reasonable ability to discern 'No Claim' instances but struggled with the accurate classification of 'Claim' cases, leading to a moderate overall accuracy. The AUC values, although slightly improved in the latter iterations, suggest a moderate discriminatory ability of the model. Further exploration into feature engineering or alternate modeling techniques may be required to enhance the model's predictive capacity for more accurate risk assessment.


## Logistic Regression –  

The logistic regression model seems to have been developed to evaluate the probability of an insurance claim based on various features within the dataset. Based on the study of coefficients, some predictors exhibited significant influence on the claim probability. It was observed that the policy tenure had a positive influence, suggesting longer policy durations tend to have higher claim probabilities. On the other hand, factors such as the age of the car, airbags, ESC, and adjustable steering had negative coefficients, indicating a decrease in the likelihood of claims associated with these features.  

However, the model had limitations in its predictive accuracy, achieving an overall accuracy of approximately **56.49%**. The confusion matrix showed significant differences in sensitivity and specificity, at **56.29%** and **59.42%**, respectively. These values indicate that the model struggled to accurately identify both 'No Claim' and 'Claim' instances. The Area Under the Curve (AUC), a metric indicating the model's discriminatory ability, was calculated at **0.6106**, suggesting only moderate performance distinguishing between positive and negative cases.  

The predicted probabilities for the first few instances of the validation dataset presented a wide range of estimates for the likelihood of claims, emphasizing the model's inconsistencies in prediction. Despite certain significant predictors, the model's overall performance indicates the need for refinement or augmentation, potentially through feature engineering or considering alternate modeling techniques to achieve more accurate and reliable predictions for insurance claim probabilities.

**CONFUSION MATRIX FOR LOGISTIC REGRESSION –**  

<img width="353" alt="image" src="https://github.com/krishnaapurva/Car-Insurance-Claim-Prediction/assets/41700695/126ae41b-a76e-4b7d-9ca8-72eb53031d21">

**ROC CURVE FOR LOGISTIC REGRESSION –**  

<img width="468" alt="image" src="https://github.com/krishnaapurva/Car-Insurance-Claim-Prediction/assets/41700695/e71e9359-a51e-478d-a4da-d429d5f94b3e">

**AREA UNDER THE CURVE: 0.6106**

## Decision Tree:  

The initial decision tree model, called default.ct, was created using the rpart algorithm and displayed moderate performance, with an accuracy of around **59.77%**, a sensitivity of **44.60%**, and a specificity of **74.93%**. However, it struggled with prediction accuracy, particularly for the positive class (1).  

On the other hand, the more complex and deeper decision tree model, named deeper.ct, had a larger number of leaf nodes (3752) and showed significantly improved accuracy, achieving an accuracy of around 96.51% and a notable improvement in both sensitivity **(93.60%)** and specificity **(99.41%)**. Nonetheless, the deeper tree appeared to be overfitting the training data due to its complexity, indicated by the extensive number of leaf nodes.  

Cross-validation was employed to optimize the tree's complexity and avoid overfitting. The cross-validated tree, pruned.ct, was pruned at a complexity parameter (cp) of 0.01, resulting in an enhanced model that balances complexity and predictive power. With fewer nodes, this pruned tree demonstrated an accuracy of **46.37%** on the validation dataset, with a sensitivity of **44.37%** and specificity of **75.40%**.

<img width="345" alt="image" src="https://github.com/krishnaapurva/Car-Insurance-Claim-Prediction/assets/41700695/fb03492d-098b-4f00-a197-fe0c83b2518f">

<img width="335" alt="image" src="https://github.com/krishnaapurva/Car-Insurance-Claim-Prediction/assets/41700695/caaad57a-d1e3-4114-a73a-73d62f3f7f1b">

The area under the receiver operating characteristic (ROC) curve was calculated for each decision tree, a metric to evaluate model performance. The initial and pruned trees had AUC values of 0.6099 and 0.6109, respectively. The ROC curves visually represented the trade-off between sensitivity and specificity for different decision thresholds, indicating modest discriminatory power for the models.  

In summary, while the deeper tree performed well on the training set, it faced overfitting issues. However, the pruned tree, optimized through cross-validation, provided a more balanced performance, although it still struggled to achieve high accuracy and sensitivity, especially for the positive class. Thus, balancing complexity and performance while selecting an appropriate model for prediction tasks is essential, ensuring accuracy and generalizability in real-world applications.  


**CONFUSION MATRIX FOR DECISION TREE –** 

<img width="457" alt="image" src="https://github.com/krishnaapurva/Car-Insurance-Claim-Prediction/assets/41700695/b8dc1f07-e8d1-4f6f-ab15-49bd7f65f1ec">

**ROC CURVE FOR DECISION TREE -** 

<img width="381" alt="image" src="https://github.com/krishnaapurva/Car-Insurance-Claim-Prediction/assets/41700695/b22405d2-9077-4b6c-aba7-8bc06a31e116">

**AREA UNDER THE CURVE: 0.6109**

## Neural Network  

Two neural network models were tested to predict 'is_claim' in the dataset. The first model had a network size of 5 nodes and produced only 7.71% accuracy. The model had difficulty distinguishing between the 'No Claim' and 'Claim' categories, as evidenced by a high false positive rate and low sensitivity. The area under the ROC curve (AUC) for this model was 0.5004, which is comparable to random chance and indicates poor predictive capability.  

To improve the model's performance, the second model was constructed with an increased network size of 10 nodes. While this led to a significant improvement in accuracy, with a score of approximately 76.71%, the model still had limitations. The confusion matrix revealed a high false positive rate, indicating a lack of specificity. The AUC value for this model only marginally increased to 0.5118, suggesting a slight improvement in discrimination ability, but still insufficient for reliable predictions. Although the second model better captured 'No Claim' instances, it struggled with 'Claim' classification.  

Overall, both neural network models showed limitations in accurately predicting 'is_claim.' The first model failed to perform better than random chance, while the second model suffered from a lack of specificity. Improving the models' performance may require addressing class imbalances, optimizing network architecture, or incorporating additional relevant features. These enhancements could lead to more reliable risk assessment in future iterations.


**CONFUSION MATRIX FOR NEURAL NETWORK-**

<img width="361" alt="image" src="https://github.com/krishnaapurva/Car-Insurance-Claim-Prediction/assets/41700695/f711d619-4432-4667-b499-da4fbaede011">

**ROC CURVE FOR NEURAL NETWORK –**  

<img width="358" alt="image" src="https://github.com/krishnaapurva/Car-Insurance-Claim-Prediction/assets/41700695/bbb89368-c47c-42fa-889f-4e2c8429ef6f">

**AREA UNDER THE CURVE: 0.5118**


## Performance Overview –

<img width="694" alt="Screenshot 2023-12-12 at 1 27 28 PM" src="https://github.com/krishnaapurva/Car-Insurance-Claim-Prediction/assets/41700695/07b3ec79-d669-4c66-9be4-b342b3d21cf2">


## Conclusion –

To summarize the analysis, we gained insightful observations about different models. Logistic regression emerged as the top performer with the highest accuracy, implying strong predictive capabilities. However, unlike the Random Forest model, its lower AUC suggests a comparatively weaker ability to distinguish between classes. Despite its lower accuracy, Random Forest demonstrated superior discrimination between positive and negative cases, as indicated by its higher AUC. However, the trade-off is its reduced interpretability compared to logistic regression. Moreover, the decision tree foundation of Random Forest is susceptible to overfitting, which may affect its generalization to new data. These findings illustrate the trade-offs between accuracy, interpretability, and generalization, which can guide model selection based on specific project requirements.



