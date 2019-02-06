# Stat628-Module1-BodyfatCalculate
### Group Members: 
Yingjing Jiang; Hongyi Jin; Wanwan Su
## Introduction:
This is a group project to come up with a simple, robust, accurate and precise method to measure the percentage of body fat based on a real data set.

## Abstract:
Body fat percentage, a measure of obesity, plays an important role in health assessment. It is helpful for people to have a basic knowledge of their health condiction. However, accurate measurement of body fat is often costly and inconvenient, while it is desirable to have easy and convenient methods of estimating body fat. 
In this module, we are going to figure out a simple and robust method to estimate percentage of body fat based on a real data set of 252 men with measurements of their percentage of body fat and various body circumference measurements.

## Background Information 

There is a method to calculate body density accurately based on determining body volume by underwater submersion. But it is hard for doctors who want to and easily quickly determine a patient’s body fat percentage based on commonly available measurements to use.

Accordind to some popular books, estimating the percentage of body fat is at least a part, considered to assess body health. The books show that age, skin-fold measurements and body circumference measurements are used to estimate body fat. Additionally, online body fat percentage calculators show that among age, skin-fold measurements and body circumference measurements, "AGE", "WEIGHT","HEIGHT", "WRIST",HIP" are often used.

The analysis will base on the fact that Percentage of body fat for an individual can be estimated once body density has been determined. Therefore, we would try to figure out if body-fat percentage is highly-related with body density.


## Data Description:
### Data Set
The data is a real collection of 252 men with measurements of their percentage of body fat and various body circumference measurements as follows:
Age (years)  
Weight (lbs)  
Height (inches)  
Adioposity (bmi)
Neck circumference (cm)  
Chest circumference (cm)  
Abdomen 2 circumference (cm)  
Hip circumference (cm)  
Thigh circumference (cm)  
Knee circumference (cm)  
Ankle circumference (cm)  
Biceps (extended) circumference (cm)  
Forearm circumference (cm)  
Wrist circumference (cm) 
Body Density(gm/cm^3)
Body Fat Percentage

>"Measurement standards are listed in Benhke and Wilmore (1974), pp. 45-48 where, for instance, the abdomen 2 circumference is measured "laterally, at the level of the iliac crests, and anteriorly, at the umbilicus."

### Equation and  Definition:
Siri (1956)) assume that the body consists of two components - lean body tissue and fat tissue.
<br> D = 1/[(A/a) + (B/b)]；B = (1/D)*[ab/(a-b)] - [b/(a-b)]
<br> D = Body Density (gm/cm^3)
<br> A = proportion of lean body tissue 
<br> B = proportion of fat tissue (A+B=1)
<br> a = density of lean body tissue (gm/cm^3) 
<br> b = density of fat tissue (gm/cm^3) 
<br>"Siri's equation":  Percentage of Body Fat (i.e. 100*B) = 495/D - 450.  

>From Siri's equation, we get the relationship between body fat percentage and  body density.

## Programming Language:
R

