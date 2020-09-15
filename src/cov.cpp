// cov.cpp
// taken from https://www.geeksforgeeks.org/program-find-covariance/
// CPP Program to find 
// covariance of two set. 
#include<bits/stdc++.h> 
using namespace std; 

// Function to find mean. 
float mean(float arr[], int n) 
{ 
  float sum = 0; 
  for(int i = 0; i < n; i++) 
    sum = sum + arr[i]; 
  return sum / n; 
} 

// Function to find covariance. 
float covariance(float arr1[], float arr2[], int n) 
{ 
  float sum = 0; 
  for(int i = 0; i < n; i++) 
    sum = sum + (arr1[i] - mean(arr1, n)) * 
    (arr2[i] - mean(arr2, n)); 
  return sum / (n - 1); 
} 
