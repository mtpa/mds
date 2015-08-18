# Evaluating Predictive Accuracy of a Binary Classifier (Python)

def evaluate_classifier(predicted, observed):
    import pandas as pd 
    if(len(predicted) != len(observed)):
        print('\nevaluate_classifier error:',\
             ' predicted and observed must be the same length\n')
        return(None) 
    if(len(set(predicted)) != 2):
        print('\nevaluate_classifier error:',\
              ' predicted must be binary\n')
        return(None)          
    if(len(set(observed)) != 2):
        print('\nevaluate_classifier error:',\
              ' observed must be binary\n')
        return(None)          

    predicted_data = predicted
    observed_data = observed
    input_data = {'predicted': predicted_data,'observed':observed_data}
    input_data_frame = pd.DataFrame(input_data)
    
    cmat = pd.crosstab(input_data_frame['predicted'],\
        input_data_frame['observed']) 
    a = float(cmat.ix[0,0])
    b = float(cmat.ix[0,1])
    c = float(cmat.ix[1,0]) 
    d = float(cmat.ix[1,1])
    n = a + b + c + d
    predictive_accuracy = (a + d)/n
    true_positive_rate = a / (a + c)
    false_positive_rate = b / (b + d)
    precision = a / (a + b)
    specificity = 1 - false_positive_rate   
    expected_accuracy = (((a + b)*(a + c)) + ((b + d)*(c + d)))/(n * n)
    kappa = (predictive_accuracy - expected_accuracy)\
       /(1 - expected_accuracy)   
    return(a, b, c, d, predictive_accuracy, true_positive_rate, specificity,\
        false_positive_rate, precision, expected_accuracy, kappa)

            
