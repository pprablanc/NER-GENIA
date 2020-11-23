# Projet d'école: Extraction d'information dans des textes

## Contexte
Ce projet de groupe s'inscrit dans le cadre d'une évaluation du cours de "Advanced Machine Learning" du Master 2 Data Mining. Ce projet a été réalisé par Bich-Ngoc HOANG, Pierre PRABLANC et Yang YANG.

## Descriptif du sujet

Ce projet consiste à traiter la tâche “term annotation” du corpus GENIA : http://www.geniaproject.org/genia-corpus/term-corpus. Celui-ci consiste en des résumés d’articles scientifiques issus de pubmeb 2 . La tâche consiste à apprendre un classifieur capable de détecter automatiquement des termes appartenant à certaines catégories (des entités nommées) telles que des noms de protéines, de cellules.... Les objectifs sont :
— Présenter dans le rapport une synthèse expliquant les fondements et propriétés des différentes méthodes d’apprentissage. Il est attendu des étudiants un bref état de l’art sur ces techniques.
— Pré-traiter les documents du corpus xml de façon à enlever les méta-données et à représenter les textes sous forme numérique selon les besoins des classifieurs.
— Pré-traiter les documents de sorte à extraire les features intéressants pour la tâche de catégorisation et selon la méthode considérée (voir [Zhou et al., 2004] par exemple).
— Traiter la tâche par les modèles probabilistes suivants : Naı̈ve Bayes (NB), Hidden Markov Models (HMM) et Conditional Random Fields (CRF). Pour cela, vous utiliserez des librairies existantes. 
— Tester ces trois modèles sur la tâche et faire une analyse critique et comparative des performances des différentes méthodes.
L’ensemble des librairies utilisées doivent être en R.
Quelques références à titre indicatif que vous compléterez à votre guise et selon les besoins de votre projet : [Sun et al., 2012, Jiang, 2012].

