function [C, sigma,errors] = dataset3Params(X, y, Xval, yval)
%EX6PARAMS returns your choice of C and sigma for Part 3 of the exercise
%where you select the optimal (C, sigma) learning parameters to use for SVM
%with RBF kernel
%   [C, sigma] = EX6PARAMS(X, y, Xval, yval) returns your choice of C and 
%   sigma. You should complete this function to return the optimal C and 
%   sigma based on a cross-validation set.
%

% You need to return the following variables correctly.
C = 1;
sigma = 0.3;

paraList=[0.01,0.03,0.1,0.3,1,3,10,30];
paraLen=length(paraList);
errors=zeros(paraLen,paraLen);
% ====================== YOUR CODE HERE ======================
% Instructions: Fill in this function to return the optimal C and sigma
%               learning parameters found using the cross validation set.
%               You can use svmPredict to predict the labels on the cross
%               validation set. For example, 
%                   predictions = svmPredict(model, Xval);
%               will return the predictions on the cross validation set.
%
%  Note: You can compute the prediction error using 
%        mean(double(predictions ~= yval))
%
minError=1000000;
for i=1:paraLen
	for j=1:paraLen
		model=svmTrain(X, y, paraList(i), @(x1, x2) gaussianKernel(x1, x2, paraList(j)));
		prediction=svmPredict(model,Xval);
		err=mean(double(prediction~=yval));
		errors(i,j)=err;
		if err<minError
			minError=err;
			C=paraList(i);
			sigma=paraList(j);
		end
	end
end





% =========================================================================

end
