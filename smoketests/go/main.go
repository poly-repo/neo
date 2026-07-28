package main

import "fmt"

func main() {
	user := NormalizeUser(User{Name: "Ada", Role: ""})

	// Intentional type error: Greeting expects the user's name, not the User.
	message := Greeting(user)
	fmt.Println(message)

	PrintSummary(user)
}
