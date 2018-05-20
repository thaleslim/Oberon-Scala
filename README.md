# Oberon-Scala
An implementation of the Oberon language in Scala

:octocat: Append and modify this list as you make changes to this repository
- [ ] unchecked example
- [x] checked example

TODOs:

 - [x] Scope:

 * Change from only storing the Variable' Value to something that stores its Current Value and its Data Type.
 
 - [x] Declaration method:

 * Adapt to fit the Scope' changes.
 
 - [ ] Global Variables && Assignment: 

 * Create a Dedicated Scope to Global Variables; 
 * Assignment method should verify if the variable already exists, if it does Throw a Exception;
 * Two variables with the same name in the same scope shouldn't be allowed;
 * The current behaviour is to override the Value.
 
 - [ ] ReadInt && ReadBool:
 
 * Create Scanf() like method.

 - [ ] FunctionsDeclaration(Class) && FunctionsCall(Expression):
 
 * Needs a way to connect the name to the CommandBlock, Map structure.
 
 - [ ] ProceduresDeclaration(Class) && ProcedureCall(Comand):
 
 * Needs a way to connect the name to the CommandBlock, Map structure.

 - [ ] Exceptions:
 
 * Create more Exceptions, like InvalidArgument, to have a more direct Error Message.
