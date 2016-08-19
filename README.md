# Spam_Detection_Project

We are only bothered by SPAM emails in our daily life. This project helps detect potential spam email with two different machine learning algorithms -- Decision Tree and K-nearest-neighbor. 

Data is already included in the first part

1. Anatomized tons of training emails into Head, Body and Attachment with Stringr package, regular expression, etc.
2. Feature Engineering: Created nearly 30 features (e.g. number of recipient, attachments, sent hour, etc) to separate SPAM and HAM. Features were summarized from email context and other information.
3. Built two models with all the created features using two different methods—k-nearest neighbors and Decision tree.
4. Predicted the training data with our models, explored the results, and improved their performance by exploring variable
transformations, applying cross-validation and selecting the best k, distance metric as well as voting mechanism.
5. Predicted the blind test data with our optimized model, the accuracy reached to nearly 90%
