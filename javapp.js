/* ------------------------------------------------------------------------------------------------- *\
	Javapp: a partial implementation of javap.
	It lists the members of the specified class. It runs in the Rhino shell.

	Copyright (c) 2015 Hugh Sweeney (hsl@pobox.com)

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

// Functions used throughout:
function jlTrim(text) {		// Strip 'java.lang.' from front of text
	if (9 == text.lastIndexOf('.') && 'java.lang.' == text.substr(0,10))
		return text.substr(10)
	else
		return text
	}

function n(text) {		// Make an (array) type name more human-friendly.
				// See http://docs.oracle.com/javase/6/docs/api/java/lang/Class.html#getName%28%29

	if ('[' != text.substr(0,1))  return jlTrim(text);	// Not an array

	var i = text.lastIndexOf('[') +1;	// Find the last bracket.
	var br = text.substr(0, i);		// All the brackets
	var bt = text.substr(i);		// The base type (code)
	br = br.split('').join(']') + ']';	// Pairify brackets, eg '[[[' becomes '[][][]'	
	var xformName = {			// As per API documentation for Class.getName()
		'Z': 'boolean',
		'B': 'byte',
		'C': 'char',
		'L': function() { return n(bt.substring(1, bt.lastIndexOf(';'))) }(),	// Classname
		'D': 'double',
		'F': 'float',
		'I': 'int',
		'J': 'long',
		'S': 'short' 
		}
	return xformName[bt.substr(0,1)] + br;	// Human-readable form of name.
	}

function modsToString(mods) {	// mods: int;
	strMods = '';

	if (md.isPublic(mods))		strMods += ' public';
	if (md.isPrivate(mods))		strMods += ' private';
	if (md.isProtected(mods))	strMods += ' protected';
	if (md.isStatic(mods))		strMods += ' static';
	if (md.isFinal(mods))		strMods += ' final';
	if (md.isSynchronized(mods))	strMods += ' synchronized';
	if (md.isVolatile(mods))	strMods += ' volatile';
	if (md.isTransient(mods))	strMods += ' transient';
	if (md.isNative(mods))		strMods += ' native';
	if (md.isInterface(mods))	strMods += ' interface';
	if (md.isAbstract(mods))	strMods += ' abstract';
	if (md.isStrict(mods))		strMods += ' strict';
	return strMods.substr(1);
	}
// End of function definitions

// Short aliases for some fully qualified Java class names:
var sys = java.lang.System;
var md = java.lang.reflect.Modifier;

// An object to hold all the class information
var classInfo = { cname: arguments[0] };

// Load the desired class
var ocl = new java.lang.Class.forName(classInfo.cname);

// Extract class information
classInfo.pkg = ocl.getPackage();
classInfo.superName = (ocl.isInterface()? null: ocl.getSuperclass().getName());
classInfo.arrIfs = ocl.getInterfaces();
classInfo.arrCons = ocl.getDeclaredConstructors();
// Extract members information
classInfo.arrFlds = ocl.getDeclaredFields();
classInfo.arrMths = ocl.getDeclaredMethods();

// Display extracted information
if (classInfo.pkg != null) sys.out.println('package '+ classInfo.pkg.getName());
sys.out.print(modsToString(ocl.getModifiers()) +' '+ classInfo.cname);		// Begin Class declaration ...
if (!ocl.isInterface()) if ('java.lang.Object' != classInfo.superName) sys.out.print(' extends '+ classInfo.superName);

if (classInfo.arrIfs.length) {			// if it implements any interfaces at all
	sys.out.print(' implements ');
	var strIfs = '', i;
	for (i=0; i<classInfo.arrIfs.length; i++) { strIfs += ', '+ classInfo.arrIfs[i].getName(); }
	sys.out.print(strIfs.substr(2));
	}

sys.out.println(' {');		// End of class declaration, start of body;

// Display constructors information
for (i=0; i<classInfo.arrCons.length; i++) {
	var con = classInfo.arrCons[i];		// Next constructor
	sys.out.print('\t'+ modsToString(con.getModifiers()) +' '+ con.getName() +'(');	// Begin constructor declaration
	var strPT = '', arrPT = con.getParameterTypes();
	for (var j=0; j<arrPT.length; j++) { strPT += ', '+ n(arrPT[j].getName()) }	// String of type names
	sys.out.print( strPT.substr(2) +')');					// Show types in constructor declaration
	var strET = '', arrET = con.getExceptionTypes();
	if (arrET.length) {
		for (j=0; j<arrET.length; j++) { strET += ', '+ arrET[j].getName() }
		sys.out.print(' throws '+ strET.substr(2));			// String of exception names
		}
	sys.out.println(';');			// End of constructor declaration
	}

// Display members information
if (classInfo.arrFlds.length) {			// Fields
	for (i=0; i<classInfo.arrFlds.length; i++) {
		var fld = classInfo.arrFlds[i];
		sys.out.println('\t'+ modsToString(fld.getModifiers())		+' '+
						n(fld.getType().getName())	+' '+
							fld.getName()		+';');
		}
	}

if (classInfo.arrMths.length) {			// Methods
	for (i=0; i<classInfo.arrMths.length; i++) {
		var mth = classInfo.arrMths[i];
		sys.out.print('\t'+ modsToString(mth.getModifiers()) +' '+ n(mth.getReturnType().getName()) +' '+
						 				mth.getName() +'(');
		strPT = '', arrPT = mth.getParameterTypes();
		for (j=0; j<arrPT.length; j++) { strPT += ', '+ n(arrPT[j].getName()) }	// String of type names
		sys.out.print( strPT.substr(2) +')');					// Show types in method declaration
		strET = '', arrET = mth.getExceptionTypes();
		if (arrET.length) {
			for (j=0; j<arrET.length; j++) { strET += ', '+ arrET[j].getName() }
			sys.out.print(' throws '+ strET.substr(2));			// String of exception names
			}
		sys.out.println(';');
		}
	}

sys.out.println('\t}');

