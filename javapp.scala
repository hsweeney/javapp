/* ------------------------------------------------------------------------------------------------- *\
	Javapp: a partial implementation of javap.
	It lists the members of the specified class. It runs in the JVM in the Scala environment.

	Copyright (c) 2017 Hugh Sweeney (hsl@pobox.com)

  This Scala version is translated from the original JS. It's probably quite bad Scala. I did it to
  get practice at writing Scala code before completely ploughing through the language specification.
  Documentation for the original JS version follows: 

	This script takes one argument on the command line: the name of the class of interest.
	No command-line switches are supported, but it is designed to operate as javap would
	operate with the '-private' switch (and only that switch) specified.

	This code was developed to look inside Java class files on a system where there was no JDK
	installed. (And therefore no javap available.) Those class files had been compiled by Rhino's
	jac compiler from my Javascript source. I needed to find out why they weren't behaving as I
	expected. And this script itself was an exercise in exploring Javascript code instantiating
	Java classes (from the JRE) within the Rhino shell.

	This code is not heavily documented as regards interaction with the JRE classes. If you want
	to understand the 'why' of what it does, you need to read it in conjunction with the vendor's
	API documentation for the Class, Constructor, Member, Field, Method and Modifier classes.
\* ------------------------------------------------------------------------------------------------- */


class CI {    //!! a remnant of the JS version
  var arrFlds: Array[java.lang.reflect.Field] = null;
  var arrMths: Array[java.lang.reflect.Method] = null;
}   // CI


object Javapp {

// Functions used throughout:
// ...............................................................................
def jlTrim(text:String) : String = {		                    // Strip 'java.lang.' from front of text
	if (9 == text.lastIndexOf('.') && "java.lang." == text.substring(0,10))
		return text.substring(10)
	else
		return text
	}
// ...............................................................................
def n(text:String) : String = {		                          // Make an (array) type name more human-friendly.
	// See http://docs.oracle.com/javase/6/docs/api/java/lang/Class.html#getName%28%29

	if ("[" != text.substring(0,1))  return jlTrim(text);	// Not an array

	var i = text.lastIndexOf("[") +1;	                        // Find the last bracket.
	var br = text.substring(0, i);		                        // All the brackets
	var bt = text.substring(i);		                            // The base type (code)
	br = br.split("").mkString("]") + "]";	// Pairify brackets, eg '[[[' becomes '[][][]'	
  def xformName(x: String) : String = x match {
		case "Z" => "boolean"
		case "B" => "byte"
		case "C" => "char"
		case "L" => n(bt.substring(1, bt.lastIndexOf(';')))     // Classname
		case "D" => "double"
		case "F" => "float"
		case "I" => "int"
		case "J" => "long"
		case "S" => "short'" 
    }
	return xformName(bt.substring(0,1)) + br;	                // Human-readable form of name.
	}
// ...............................................................................
  def modsToString(mods:Int) : String = {                   // Human-readable modifiers
  	var strMods = "";

  	if (java.lang.reflect.Modifier.isPublic(mods))		    strMods += " public";
  	if (java.lang.reflect.Modifier.isPrivate(mods))		    strMods += " private";
  	if (java.lang.reflect.Modifier.isProtected(mods))	    strMods += " protected";
  	if (java.lang.reflect.Modifier.isStatic(mods))		    strMods += " static";
  	if (java.lang.reflect.Modifier.isFinal(mods))		      strMods += " final";
  	if (java.lang.reflect.Modifier.isSynchronized(mods))	strMods += " synchronized";
  	if (java.lang.reflect.Modifier.isVolatile(mods))	    strMods += " volatile";
  	if (java.lang.reflect.Modifier.isTransient(mods))	    strMods += " transient";
  	if (java.lang.reflect.Modifier.isNative(mods))		    strMods += " native";
  	if (java.lang.reflect.Modifier.isInterface(mods))	    strMods += " interface";
  	if (java.lang.reflect.Modifier.isAbstract(mods))	    strMods += " abstract";
  	if (java.lang.reflect.Modifier.isStrict(mods))		    strMods += " strict";
  	return if (strMods == "") "" else strMods.substring(1);
	}
// End of function definitions
// ...............................................................................
  
  def main(args: Array[String]) : Unit = {
  //  ----

  // Load the desired class
  val cname = args(0);
  val ocl = new ClassGetter().getByName(cname);

  // Extract class information
  val pkg = ocl.getPackage;
  val superName = (if (ocl.isInterface) null else ocl.getSuperclass.getName)
  val arrIfs = ocl.getInterfaces;
  val arrCons = ocl.getDeclaredConstructors;

  // Extract members information
  val classInfo = new CI()
  classInfo.arrFlds = ocl.getDeclaredFields;
  classInfo.arrMths = ocl.getDeclaredMethods;

  // Display extracted information
  if (pkg != null) println("package "+ pkg.getName);
  println(modsToString(ocl.getModifiers) +" "+ cname);		// Begin Class declaration ...
  if (!ocl.isInterface) if ("java.lang.Object" != superName) print(" extends "+ superName);

  if (arrIfs.length > 0) {			// if it implements any interfaces at all
  	print(" implements ");
  	var strIfs = "";
  	for (x <- arrIfs) strIfs += ", "+ x.getName
  	println(strIfs.substring(2));
  	}

  println(" {");		                              // End of class declaration, start of body;
  
  // Display constructors information
  for (x <- arrCons) {
  	print('\t'+ modsToString(x.getModifiers) +" "+ x.getName +"(");	    // Begin constructor declaration
  	var strPT = "";
  	for (y <- x.getParameterTypes) strPT += ", "+ n(y.getName)	        // String of type names
  	print( strPT.substring(2) +")");					                          // Show types in constructor declaration
  	var strET = "";
    for (z <- x.getExceptionTypes) strET += ", "+ z.getName
  	if (strET != "") print(" throws "+ strET.substring(2));			        // String of exception names
  	println(";");			                                                  // End of constructor declaration
	}

  // Display members information
	for (fld <- classInfo.arrFlds) {                                      // Fields
		println('\t'+ modsToString(fld.getModifiers) +" "+ n(fld.getType.getName)	+" "+ fld.getName	+";");
		}

	for (mth <- classInfo.arrMths) {                                      // Methods
    var strMods = modsToString(mth.getModifiers)
		print("\t"+ strMods + (if (strMods !="") " " else "") + n(mth.getReturnType.getName) +" "+ mth.getName +"(");
		var strPT = "";
		for (pt <- mth.getParameterTypes) strPT += ", "+ n(pt.getName)	                    // String of type names
		if (strPT != "") print( strPT.substring(2)); print(")")					                    // Show types in method declaration
		var strET = "";
		for (et <- mth.getExceptionTypes) strET += ", "+ et.getName
		if (strET != "") print(" throws "+ strET.substring(2));			                        // String of exception names
		println(";");
		}
  println(" }")
  }   // main()

}   // Javapp
