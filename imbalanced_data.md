## Notes to imbalanced dataset

### Imbalanced data in traffic accidents

An imbalanced dataset is a dataset where one class outnumbers other class by
a large proportion. Imbalanced datasets do not contain enough information on
minority class for an algorithm to make an accurate prediction. It is 
desirable to use classification algorithms with balanced datasets.

Car accident dataset for Saint-Petersburg is imbalanced. Prevalance of serious
accidents is 14.1 percent. Decision tree or random forest algorithms trained on
this dataset return insufficient levels of sensitivity: 38 and 50 percent
respectively.

### Dealing with imbalanced data

Common way to deal with imbalanced datasets is to modify data into balanced
distribution using an algorithm. There are four algorithms available in `R`
for balancing purpose:  
- undersampling;  
- oversampling;  
- synthetic data generation;  
- cost sensitive learning.  

Undersampling method randomly kicks majority class observations from a dataset
until both majority and minority classes are balanced. This method is 
applicable to large datasets where reducing the number of observations benefit
to training time. The flip side of this approach is that you might miss
important features of majority class.

Oversampling method randomly replicates minority class observations. It is good
for relatively small datasets when you do not want to loose any valuable 
information. The not-so-good part of the method is that it only adds 
observations of certain types, which leads to overfitting.

Synthetic data generation creates artificial dataset based on features 
similarities. This method creates many different combinations of observed
minority class features. The method is good as it does not lead to overfitting.

Cost sensitive learning method calculates the cost of error classification for
*FP* and *FN* - both costs result from multiplication of number of false
classifications by their costs. Total cost tend to minimum.








