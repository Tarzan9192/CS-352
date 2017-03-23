# CS-352
# Simple Thing Recognizer in Prolog

The purpose of this project is to write in Prolog a progarm capable of learning and answering simple questions about the properties of things. 

# Project Description
The program will utilize the execute/2 predicate to take in queries and statements from the user regarding the properties of things the program may or may not know about. If the program contains matching information in its knowledge base, it responds appropriately. Similarily, when it has no prior knowledge of thing mentioned, it may then be added to the knowledge base of the program to be referred to in future queries.

# Sample Interaction
User:   What is the color of the car?
Prolog: I don't know.

User:   The color of the car is blue.
Prolog: Ok.

User:   The color of the car is green.
Prolog: It's not.
