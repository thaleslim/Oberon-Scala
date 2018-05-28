# Oberon-Scala
An implementation of the Oberon language in Scala

:octocat: Append and modify this list as you make changes to this repository
- [ ] unchecked example
- [x] checked example

TODOs:

 - [ ] FunctionsDeclaration(Class) && FunctionsCall(Expression):
 
 * Needs a way to connect the name to the CommandBlock, Map structure.
 * Hint: unlock the access to CommandBlock' commands to 
   be able to evaluate if it's a return command, to
   help with FunctionsCall' eval method.

 - [ ] ReadInt && ReadBool:
 
 * Create Scanf() like method.

 - [ ] Print:

 * Possible types expansion (String).

 - [ ] Declaration:

 * Improve method' arguments usability.

 - [ ] CommandBlock:

 * Use Sequences instead of Lists.

 - [ ] Design decision:

 * "Brackets" (begin [...] end) should be obrigatory?

 - [ ] Procedure:

 * Review Hierarchy and Class' acess.

 - [ ] Exceptions:
 
 * Create more Exceptions, like InvalidArgument, to have a more direct Error Message.

 - [ ] Review identation in Command.scala