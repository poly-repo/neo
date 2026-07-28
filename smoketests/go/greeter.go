package main

import "fmt"

// User is the person shown by the smoke-test program.
type User struct {
	Name string
	Role string
}

// Greeting returns a short welcome message for a name.
func Greeting(name string) string {
	return fmt.Sprintf("Welcome, %s!", name)
}

// legacyGreeting is intentionally unused so staticcheck reports U1000.
func legacyGreeting(name string) string {
	return "Hello, " + name
}
