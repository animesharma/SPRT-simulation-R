sequential_probability_ratio_test = function(input_sequence, alpha, beta, theta) {
	n = length(input_sequence)
	input_sequence_sum = sum(input_sequence)
	
	# Unpack values of theta
	theta0 = theta[1]
	theta1 = theta[2]

	# Calculate the lower and upper bounds for the stopping threshold
	a = log(beta / (1 - alpha))
	b = log((1 - beta) / alpha)

	# Calculate L(theta) for Bernoulli
	L_theta0 = (theta0 ^ input_sequence_sum) * ((1 - theta0) ^ (n - input_sequence_sum))
	L_theta1 = (theta1 ^ input_sequence_sum) * ((1 - theta1) ^ (n - input_sequence_sum))

	# Calculate the cumulative sum of the log-likelihood ratio
	cllr = log(L_theta1 / L_theta0)

	# Return -1, 0 or 1 based on the values of 'cllr', 'a' and 'b'
	if (cllr <= a) return (-1)
	if (cllr >= b) return (1)
	return (0)
}

simulate_sprt = function(p) {
	# Define Null and Alternate Hypotheses
	H0 = "P <= 0.45"
	H1 = "P >= 0.55"

	# Initialize the values of parameters
	iterations = 0
	test_sequence = c()
	theta = c(0.45, 0.55)
	alpha = 0.01
	beta = 0.01
	stop_iteration = 0

	# Iterate sequentially till stopping condition is reached
	while(stop_iteration == 0) {
		# Add a new random bernoulli trial to the test sequence 
		test_sequence = append(test_sequence, rbinom(1, 1, p))
		# Update the value of 'stop_iteration' flag based on the function return value
		stop_iteration = sequential_probability_ratio_test(test_sequence, alpha, beta, theta)
		# Increment the number of iterations
		iterations = iterations + 1
	}

	# Print result
	if (stop_iteration == -1) cat("Iterations: ", iterations, "\nAccept: ", H0, ", Reject: ", H1)
	if (stop_iteration == 1) cat("Iterations: ", iterations, "\nAccept: ", H1, ", Reject: ", H0)
}

# Run simulation for various values of P
simulate_sprt(0.3)
simulate_sprt(0.56)
simulate_sprt(0.54)
