def classify(features_train, labels_train):
    from sklearn.naive_bayes import GaussianNB
    classifier = GaussianNB()
    classifier.fit(features_train, labels_train)
    return classifier
