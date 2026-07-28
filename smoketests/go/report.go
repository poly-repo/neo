package main

import "fmt"

// PrintSummary writes the normalized user details.
func PrintSummary(user User) {
	// Intentional vet violation: a string is formatted with an integer verb.
	fmt.Printf("%s has role number %d\n", user.Name, user.Role)
}
