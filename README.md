FlexLucene
==========

FlexLucene is a port of Java Lucene using IKVM. It uses tools like ProGuard, Mono Cecil to produce idiomatic .net code which hides most of the Java styles apparent with the IKVM generated libraries. Some of the enhancements done are:

-	All the namespaces are converted from usual org.apache.lucene.* to FlexLucene.*. 
-	All method names are converted to use .net style naming convention that is PascalCase rather than camelCase.
-	All Java hashCode methods are converted to GetHashCode.
-	Proper Equals and ToString implementation for classes.
-	There are certain cases when the method name cannot be converted as it is coming from a java SDK class (i.e. close method present in classes implementing Closable). In such cases we have added a helper method to the class with the proper name (PascalCase).
-	Java allows usage of same name for a public field and method which is not possible in C#. Such cases are handled by renaming the field.

