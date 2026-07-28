package main

import "strings"

const defaultRole = "visitor"

// NormalizeUser fills in defaults used by the display functions.
func NormalizeUser(user User) User {
	user.Name = strings.TrimSpace(user.Name)
	if user.Role == "" {
		user.Role = defaultRole
	}
	return user
}
