/++
Implements RFC 6570 (URI Template)

Standards: https://tools.ietf.org/html/rfc6570

Examples:
---
import uritemplate;

string uriTemplate = "https://cool.webfreak.org/{user}/profile{?fields*}";

assert(uriTemplate.expandTemplateURIString([
	"user": URIVariable("bob"),
	"fields": URIVariable([
		"address", "name", "birthday"
	])
]) == "https://cool.webfreak.org/bob/profile?fields=address&fields=name&fields=birthday");

assert(uriTemplate.expandTemplateURIString([
	"user": URIVariable(["bob!", "Straße"]),
	"fields": URIVariable([
		"address", "name", "birthday"
	])
]) == "https://cool.webfreak.org/bob%21,Stra%C3%9Fe/profile?fields=address&fields=name&fields=birthday");
---
+/
module uritemplate;

import std.algorithm;
import std.array;
import std.ascii;
import std.conv;
import std.range;
import std.string;
import std.typecons;
import std.utf;

@safe:

unittest
{
	// documentation unittest
	string uriTemplate = "https://cool.webfreak.org/{user}/profile{?fields*}";
	assert(uriTemplate.expandTemplateURIString([
		"user": URIVariable("bob"),
		"fields": URIVariable([
			"address", "name", "birthday"
		])
	]) == "https://cool.webfreak.org/bob/profile?fields=address&fields=name&fields=birthday");
	assert(uriTemplate.expandTemplateURIString([
		"user": URIVariable(["bob!", "Straße"]),
		"fields": URIVariable([
			"address", "name", "birthday"
		])
	]) == "https://cool.webfreak.org/bob%21,Stra%C3%9Fe/profile?fields=address&fields=name&fields=birthday");
}

/// Exception thrown on malformed URI templates
class TemplateURIFormatException : Exception
{
	/// Index at which point in the input URI template this error occurred.
	size_t index;

	///
	this(size_t index, string msg, string file = __FILE__, size_t line = __LINE__,
			Throwable nextInChain = null) pure nothrow @nogc @safe
	{
		this.index = index;
		super(msg, file, line, nextInChain);
	}
}

/// Tagged union for template URI variables.
/// Implementation tags the length of the arrays for the type.
struct URIVariable
{
	/// Describes the possible types a variable can have.
	enum Type
	{
		/// A single string value.
		value,
		/// A string array.
		array,
		/// A string -> string associative array.
		map
	}

	enum undefined = URIVariable.init;

	enum TagBitLength = 2;
	enum TagBitMask = ~((cast(size_t)-1) << TagBitLength);

	// pointer to a map or pointer to the array .ptr
	void* ptr;

	// tagged length (tag in least significant `TagBitLength` bits)
	size_t rawLength;

	/// Initializes this variable to a string value.
	this(string value) @safe
	{
		this.value = value;
	}

	/// Initializes this variable to an array value. All items are assumed to
	/// be defined, so null strings are equivalent to empty strings.
	this(string[] array) @safe
	{
		this.array = array;
	}

	/// Initializes this variable to a map value. All items are assumed to be
	/// defined, so null strings are equivalent to empty strings.
	this(Tuple!(string, string)[] map) @safe
	{
		this.map = map;
	}

	/// ditto
	this(string[string] map) @safe
	{
		this.map = map;
	}

	/// Returns: whether this variable is undefined or not.
	/// A variable is undefined if it is the empty list or unset. Empty string
	/// is not considered undefined.
	bool isUndefined() const @property @safe
	{
		final switch (type)
		{
		case Type.value:
			return ptr is null && length == 0;
		case Type.array:
		case Type.map:
			return length == 0;
		}
	}

	/// Clears this variable back to being undefined.
	void clear() @safe
	{
		ptr = null;
		rawLength = 0;
	}

	/// Returns: whether this variable is a single value, array or map.
	///          Note that if `isUndefined` is true this is of type string
	///          anyway, potentially violating RFC 6570 section 2.3 if not
	///          checked.
	Type type() const @property @trusted
	{
		return cast(Type)(rawLength & TagBitMask);
	}

	/// The length of a value, count of an array or count of a map.
	size_t length() const @property @safe
	{
		return rawLength >> TagBitLength;
	}

	/// Returns: true if this is an empty list or empty string.
	bool isEmpty() const @property @safe
	{
		return (rawLength & ~TagBitMask) == 0;
	}

	/// Returns: the single value this variable is pointing to.
	string value() const @property @trusted
	in(type == Type.value,
			"Attempted to access variable of type " ~ type.to!string ~ " as singular value.")
	{
		return (cast(immutable(char)*) ptr)[0 .. length];
	}

	/// Sets the type of this variable to value and sets the data.
	/// Params:
	///   value = The (new) value to set this variable to.
	/// Returns: The given value.
	string value(string value) @property @trusted
	{
		ptr = cast(void*) value.ptr;
		if (ptr is null)
			ptr = cast(void*) someEmptyString.ptr; // make sure we are not undefined
		rawLength = (value.length << TagBitLength) | Type.value;
		return value;
	}

	/// Returns: the string array this variable is pointing to.
	inout(string[]) array() inout @property @trusted
	in(type == Type.array, "Attempted to access variable of type " ~ type.to!string ~ " as array.")
	{
		return (cast(inout(string)*) ptr)[0 .. length];
	}

	/// Sets the type of this variable to array and sets the data.
	/// Params:
	///   array = The (new) array to set this variable to.
	/// Returns: The given array.
	string[] array(string[] array) @property @trusted
	{
		ptr = array.ptr;
		rawLength = (array.length << TagBitLength) | Type.array;
		return array;
	}

	/// Returns: the associative array this variable is pointing to.
	inout(Tuple!(string, string)[]) map() inout @property @trusted
	in(type == Type.map, "Attempted to access variable of type " ~ type.to!string ~ " as map.")
	{
		return (cast(inout(Tuple!(string, string))*) ptr)[0 .. length];
	}

	/// Sets the type of this variable to map and sets the data.
	/// Params:
	///   map = The (new) map to set this variable to.
	/// Returns: The given map.
	Tuple!(string, string)[] map(Tuple!(string, string)[] map) @property @trusted
	{
		ptr = map.ptr;
		rawLength = (map.length << TagBitLength) | Type.map;
		return map;
	}

	/// ditto
	Tuple!(string, string)[] map(string[string] map) @property @trusted
	{
		auto t = map.byKeyValue.map!(a => tuple(a.key, a.value)).array;
		ptr = t.ptr;
		rawLength = (t.length << TagBitLength) | Type.map;
		return t;
	}
}

///
unittest
{
	URIVariable value = URIVariable("hello");
	URIVariable array = URIVariable(["hello", "world"]);
	URIVariable map = URIVariable([tuple("foo", "bar")]);
	// AA converts to tuple list at unspecified order
	URIVariable mapAA = URIVariable(["foo": "baz"]);

	assert(!value.isUndefined);
	assert(!value.isEmpty);

	assert(!array.isUndefined);
	assert(!array.isEmpty);

	assert(!map.isUndefined);
	assert(!map.isEmpty);

	assert(!mapAA.isUndefined);
	assert(!mapAA.isEmpty);

	assert(value.type == URIVariable.Type.value);
	assert(array.type == URIVariable.Type.array);
	assert(map.type == URIVariable.Type.map);
	assert(mapAA.type == URIVariable.Type.map);

	assert(value.value == "hello");
	assert(array.array == ["hello", "world"]);
	assert(map.map == [tuple("foo", "bar")]);
	assert(mapAA.map == [tuple("foo", "baz")]);

	URIVariable undefined;
	assert(undefined.isUndefined);
	assert(undefined.isEmpty);

	URIVariable empty = URIVariable(cast(string) null);
	assert(!empty.isUndefined);
	assert(empty.isEmpty);

	URIVariable emptyList = URIVariable(cast(string[]) null);
	assert(emptyList.isUndefined);
	assert(emptyList.isEmpty);

	value.clear();
	assert(value.isUndefined);
	assert(value.isEmpty);
}

/// Callback to resolve a URIVariable by a given variable name.
/// The variable name is always resolved to a simple string and the returned
/// value should not be encoded or otherwise processed for the URI.
/// To conform to RFC 6570 the callback MUST always produce the same variables
/// for any given variable name. The values MUST be determined before template
/// expansion.
alias VariableCallback = const(URIVariable) delegate(string variableName) @safe;

/// Expands a URI template as defined in RFC 6570.
/// Params:
///   templateUri     = Given URI template as defined in RFC 6570.
///   resolveVariable = Callback delegate to resolve a variable name
///                     (parameter) to the variable value. (return value) May
///                     be called more than once per variable name.
///                     See $(REF VariableCallback). Values are percent encoded
///                     by this function and must not be encoded by the
///                     callback function.
///
///                     Note that all values returned by this function MUST be
///                     formed prior to template expansion in order to comply
///                     with RFC 6570. Therefore to ensure compatibility a
///                     given variable name must always evaluate to the same
///                     value every time.
///   strict          = Validate entire string strictly according to RFC 6570.
///                     Does NOT perform unicode code-point validation.
///                     Performs character-by-character checks for exact
///                     grammar checks.
/// Returns: The expanded URI (GC-allocated) or if there is no template in the
///          URI, the templateUri parameter as given.
/// Throws: $(LREF TemplateURIFormatException) on attempted use of a reserved
///         operator, invalid variable specifications or strict issues.
string expandTemplateURIString(scope return string templateUri,
		scope VariableCallback resolveVariable, bool strict = false)
{
	if (templateUri.indexOf('{') == -1)
	{
		if (strict)
			validateTemplateURILiteral(templateUri, 0);
		return templateUri;
	}

	auto ret = appender!string;

	ptrdiff_t last;
	while (true)
	{
		ptrdiff_t start = templateUri.indexOf('{', last);
		if (start == -1)
			break;

		if (strict)
			validateTemplateURILiteral(templateUri[last .. start], last);

		ret ~= templateUri[last .. start];

		last = templateUri.indexOf('}', start);
		if (last == -1)
			throw new TemplateURIFormatException(start,
					"Missing closing brace for template parameter");
		last++;

		auto templateString = templateUri[start + 1 .. last - 1];
		ret ~= expandTemplateURIVariable(templateString, resolveVariable, strict, start + 1);
	}

	if (strict)
		validateTemplateURILiteral(templateUri[last .. $], last);

	ret ~= templateUri[last .. $];

	return ret.data;
}

/// ditto
string expandTemplateURIString(string templateUri, scope const URIVariable[string] variables, bool strict = false)
{
	return expandTemplateURIString(templateUri,
			delegate(string k) @safe => variables.get(k, URIVariable.init), strict);
}

/// ditto
string expandTemplateURIString(string templateUri, scope const string[string] variables, bool strict = false)
{
	return expandTemplateURIString(templateUri, delegate(string k) @safe {
		if (auto v = k in variables)
			return URIVariable(*v);
		else
			return URIVariable.init;
	}, strict);
}

private static immutable nullDelegate = delegate(string k) @safe => URIVariable.init;
/// ditto
string expandTemplateURIString(string templateUri, typeof(null) variables, bool strict = false)
{
	return expandTemplateURIString(templateUri, nullDelegate, strict);
}

///
@safe unittest
{
	assert(expandTemplateURIString(`/notifications{?since,all,participating}`,
			delegate(string k) => URIVariable.init) == "/notifications");

	assert(expandTemplateURIString(`/notifications{?since,all,participating}`,
			null) == "/notifications");

	assert(expandTemplateURIString(`/notifications{?since,all,participating}`,
			["all": "1"]) == "/notifications?all=1");

	assert(expandTemplateURIString(`/notifications{?since,all,participating}`,
			["all": "1", "participating": "1"]) == "/notifications?all=1&participating=1");
}

/// Expands a variable template part of a URI template as defined in RFC 6570.
/// Params:
///   templateString  = The template variable definition without surrounding
///                     braces.
///   resolveVariable = Callback delegate to resolve a variable name
///                     (parameter) to the variable value. (return value) May
///                     be called more than once per variable name.
///                     See $(REF VariableCallback). Values are percent encoded
///                     by this function and must not be encoded by the
///                     callback function.
///
///                     Note that all values returned by this function MUST be
///                     formed prior to template expansion in order to comply
///                     with RFC 6570. Therefore to ensure compatibility a
///                     given variable name must always evaluate to the same
///                     value every time.
///   strict          = Validate entire string strictly according to RFC 6570.
///                     Does NOT perform unicode code-point validation.
///                     Performs character-by-character checks for exact
///                     grammar checks.
///   index           = Byte index to offset malformed URI format exceptions.
/// Returns: the resolved variable value (properly URI encoded)
/// Throws: $(LREF TemplateURIFormatException) on attempted use of a reserved
///         operator, invalid variable specifications or strict issues.
string expandTemplateURIVariable(string templateString,
		VariableCallback resolveVariable, bool strict = false, size_t index = 0)
{
	if (!templateString.length)
		throw new TemplateURIFormatException(index, "Empty template string");

	char op = templateString[0];
	switch (op)
	{
	case '+':
	case '#':
	case '.':
	case '/':
	case ';':
	case '?':
	case '&':
		return serializeVariables(templateString[1 .. $], resolveVariable, op, strict, index);
	case '=':
	case ',':
	case '!':
	case '@':
	case '|':
		throw new TemplateURIFormatException(index,
				"Attempted use of a reserved URI template variable operator");
	default:
		if (strict && !(op.isAlphaNum || op == '_' || op == '%'))
			throw new TemplateURIFormatException(index,
					"Attempted use of an unknown URI template variable operator");
		return serializeVariables(templateString, resolveVariable, char.init, strict, index);
	}
}

/// ditto
string expandTemplateURIVariable(string templateString,
		URIVariable[string] variables, bool strict = false, size_t index = 0)
{
	return expandTemplateURIVariable(templateString,
			delegate(string k) @safe => variables.get(k, URIVariable.init), strict, index);
}

/// ditto
string expandTemplateURIVariable(string templateString, string[string] variables,
		bool strict = false, size_t index = 0)
{
	return expandTemplateURIVariable(templateString, delegate(string k) @safe {
		if (auto v = k in variables)
			return URIVariable(*v);
		else
			return URIVariable.init;
	}, strict, index);
}

/// ditto
string expandTemplateURIVariable(string templateString, typeof(null) variables,
		bool strict = false, size_t index = 0)
{
	return expandTemplateURIVariable(templateString,
			delegate(string k) @safe => URIVariable.init, strict, index);
}

///
@safe unittest
{
	assert(expandTemplateURIVariable(`?since,all,participating`,
			delegate(string k) @safe => URIVariable.init) == "");

	assert(expandTemplateURIVariable(`?since,all,participating`, null) == "");

	assert(expandTemplateURIVariable(`?since,all,participating`, ["all": "1"]) == "?all=1");

	assert(expandTemplateURIVariable(`?since,all,participating`, [
				"all": URIVariable("1"),
				"participating": URIVariable("1")
			]) == "?all=1&participating=1");
}

/// Checks if the given literal parameter is valid according to RFC 6570
/// section 2.1.
/// Returns: `false` if invalid with errorIndex set to the first character
///          breaking the validity. Potentially end-of-string index for
///          unterminated percent-encoded characters.
bool isValidTemplateURILiteral(string literal, out size_t errorIndex)
{
	int inPercent = 0;
	foreach (i, char c; literal)
	{
		if (inPercent)
		{
			if (!c.isHexDigit)
			{
				errorIndex = i;
				return false;
			}
			inPercent--;
		}
		else
		{
			switch (c)
			{
				//dfmt off
			case 0: .. case 0x20:
			//dfmt on
			case '"':
			case '\'':
			case '<':
			case '>':
			case '\\':
			case '^':
			case '`':
			case '{':
			case '|':
			case '}':
				errorIndex = i;
				return false;
			case '%':
				inPercent = 2;
				break;
			default:
				break;
			}
		}
	}
	errorIndex = literal.length;
	return !inPercent;
}

///
@safe unittest
{
	size_t error;

	assert(isValidTemplateURILiteral("hello", error));

	assert(!isValidTemplateURILiteral("hello world", error));
	assert(error == 5);
}

/// Checks if the given literal parameter is valid according to RFC 6570
/// section 2.1.
/// Throws: $(LREF TemplateURIFormatException) in case of malformed characters.
void validateTemplateURILiteral(string literal, size_t index)
{
	size_t offset;
	if (!isValidTemplateURILiteral(literal, offset))
		throw new TemplateURIFormatException(index + offset, "Malformed template URI literal");
}

/// Checks if the given literal parameter is valid according to RFC 6570
/// section 2.3.
/// Returns: `false` if invalid with errorIndex set to the first character
///          breaking the validity. Potentially end-of-string index for
///          unterminated percent-encoded characters.
bool isValidTemplateURIVariableName(string varname, out size_t errorIndex)
{
	bool allowDot;
	int inPercent = 0;
	foreach (i, char c; varname)
	{
		if (inPercent)
		{
			if (!c.isHexDigit)
			{
				errorIndex = i;
				return false;
			}
			inPercent--;
		}
		else
		{
			if (c == '%')
			{
				inPercent = 2;
				allowDot = true;
			}
			else if (c == '.')
			{
				if (!allowDot)
				{
					errorIndex = i;
					return false;
				}
				allowDot = false;
			}
			else if (!c.isAlphaNum && c != '_')
			{
				errorIndex = i;
				return false;
			}
			else
			{
				allowDot = true;
			}
		}
	}
	errorIndex = varname.length;
	return !inPercent && allowDot;
}

/// Checks if the given literal parameter is valid according to RFC 6570
/// section 2.3.
/// Throws: $(LREF TemplateURIFormatException) in case of malformed characters.
void validateTemplateURIVariableName(string varname, size_t index)
{
	size_t offset;
	if (!isValidTemplateURIVariableName(varname, offset))
		throw new TemplateURIFormatException(index + offset, "Malformed template URI variable name");
}

private:

static immutable string someEmptyString = "\0";

struct VariableRef
{
	string name;
	ushort maxLengthCodepoints;
	bool explode;

	this(string spec, bool strict = false, size_t index = 0)
	{
		if (!spec.length)
			throw new TemplateURIFormatException(index, "Empty variable name definition");

		if (spec.endsWith('*'))
		{
			explode = true;
			name = spec[0 .. $ - 1];
		}
		else
		{
			auto colon = spec.indexOf(':');
			if (colon == -1)
			{
				name = spec;
			}
			else
			{
				name = spec[0 .. colon];
				auto maxLenSpec = spec[colon + 1 .. $];
				if (maxLenSpec.length > 4)
					throw new TemplateURIFormatException(index + colon,
							"Variable max-length too large, must be at most 4 characters");
				if (maxLenSpec.length == 0)
					throw new TemplateURIFormatException(index + colon,
							"Variable max-length after colon is missing");
				if (maxLenSpec[0] < '1' || maxLenSpec[0] > '9')
					throw new TemplateURIFormatException(index + colon + 1,
							"Variable max-length invalid starting character");
				if (!maxLenSpec[1 .. $].all!isDigit)
					throw new TemplateURIFormatException(index + colon + 1,
							"Variable max-length invalid number characters");
				maxLengthCodepoints = maxLenSpec.to!ushort;
			}
		}

		if (strict)
			validateTemplateURIVariableName(name, index);
	}
}

/// Trims the given value to the maximum length or returns the string as-is if
/// maxLength is 0. Doesn't perform any percent-decoding, so the trim must be
/// performed before percent encoding the variable.
T[] trimVariableToLength(T)(T[] value, ushort maxLength)
{
	assert(maxLength <= 9999, "maxlength too long (can only input at most 9999)");
	if (maxLength == 0)
		return value;

	size_t index = 0;
	while (maxLength-- && index < value.length)
		decode(value, index);
	return value[0 .. index];
}

unittest
{
	assert(trimVariableToLength("hello", 0) == "hello");
	assert(trimVariableToLength("hello", 1) == "h");
	assert(trimVariableToLength("hello", 9999) == "hello");

	assert(trimVariableToLength("あああ", 3) == "あああ");
	assert(trimVariableToLength("あああ", 2) == "ああ");
	assert(trimVariableToLength("あああ", 1) == "あ");
	assert(trimVariableToLength("aあああ", 2) == "aあ");
}

/// Returns: a range of $(LREF VariableRef)
auto splitVariables(T)(T[] definition, bool strict = false, size_t index = 0)
{
	if (!definition.length)
		throw new TemplateURIFormatException(index, "Empty variable name definition");

	return definition.splitter(',').map!(delegate(a) {
		auto v = VariableRef(a, strict, index);
		index += a.length + 1;
		return v;
	});
}

string serializeVariables(string spec, VariableCallback resolveVariable,
		char variableType, bool strict, size_t index)
{
	auto variables = splitVariables(spec, strict, index);
	auto ret = appender!string;
	auto sep = getListSeparatorForType(variableType);

	bool first = true;
	foreach (variable; variables)
	{
		auto value = resolveVariable(variable.name);
		if (value.isUndefined)
			continue;

		if (first)
		{
			ret ~= getStringStartForType(variableType);
			first = false;
		}
		else
		{
			ret ~= sep;
		}

		ret ~= serializeVariable(variable, value, variableType);
	}

	return ret.data;
}

/// Converts a given variable to its string form, percent encoded.
string serializeVariable(scope const VariableRef variable, scope const URIVariable value, char variableType = char.init)
{
	const string ifemp = variableType == '?' || variableType == '&' ? "=" : "";
	const bool named = variableType == '?' || variableType == '&' || variableType == ';';

	final switch (value.type)
	{
	case URIVariable.Type.value:
		if (named)
		{
			if (value.isEmpty)
				return variable.name.encodeLiteral ~ ifemp;
			else
				return variable.name.encodeLiteral ~ '=' ~ trimVariableToLength(value.value,
						variable.maxLengthCodepoints).encodeByType(variableType);
		}
		else
		{
			return trimVariableToLength(value.value, variable.maxLengthCodepoints).encodeByType(
					variableType);
		}
	case URIVariable.Type.array:
	case URIVariable.Type.map:
		auto ret = appender!string;
		auto sep = getListSeparatorForType(variableType);
		if (variable.explode)
		{
			if (named)
			{
				if (value.type == URIVariable.Type.array)
				{
					bool first = true;
					foreach (v; value.array)
					{
						if (first)
							first = false;
						else
							ret ~= sep;

						ret ~= variable.name.encodeLiteral;
						if (v.length)
						{
							ret ~= '=';
							ret ~= v.encodeByType(variableType);
						}
						else
						{
							ret ~= ifemp;
						}
					}
				}
				else
				{
					bool first = true;
					foreach (t; value.map)
					{
						auto k = t[0];
						auto v = t[1];
						if (first)
							first = false;
						else
							ret ~= sep;

						ret ~= k.encodeLiteral;
						if (v.length)
						{
							ret ~= '=';
							ret ~= v.encodeByType(variableType);
						}
						else
						{
							ret ~= ifemp;
						}
					}
				}
			}
			else
			{
				if (value.type == URIVariable.Type.array)
					return value.array.map!(a => a.encodeByType(variableType)).join(sep);
				else
					return value.map.map!(a => a[0].encodeByType(
							variableType) ~ '=' ~ a[1].encodeByType(variableType)).join(sep);
			}
		}
		else
		{
			if (named)
			{
				ret ~= variable.name.encodeLiteral;
				// can't have empty values here because empty arrays are considered undefined
				ret ~= '=';
			}

			if (value.type == URIVariable.Type.array)
			{
				bool first = true;
				foreach (v; value.array)
				{
					if (first)
						first = false;
					else
						ret ~= ',';

					ret ~= v.encodeByType(variableType);
				}
			}
			else
			{
				bool first = true;
				foreach (t; value.map)
				{
					auto k = t[0];
					auto v = t[1];
					if (first)
						first = false;
					else
						ret ~= ',';

					ret ~= k.encodeByType(variableType);
					ret ~= ',';
					ret ~= v.encodeByType(variableType);
				}
			}
		}
		return ret.data;
	}
}

string getListSeparatorForType(char variableType = char.init)
{
	switch (variableType)
	{
	case '.':
		return ".";
	case '/':
		return "/";
	case ';':
		return ";";
	case '?':
	case '&':
		return "&";
	case char.init:
	case ' ':
	case '+':
	case '#':
	default:
		return ",";
	}
}

string getStringStartForType(char variableType = char.init)
{
	switch (variableType)
	{
	case '.':
		return ".";
	case '/':
		return "/";
	case ';':
		return ";";
	case '?':
		return "?";
	case '&':
		return "&";
	case '#':
		return "#";
	case char.init:
	case '+':
	default:
		return "";
	}
}

string encodeByType(string text, char variableType = char.init)
{
	switch (variableType)
	{
	case '+':
	case '#':
		return text.encodeLiteral;
	default:
		return text.encodeUnreserved;
	}
}

string encodeLiteral(string text)
{
	auto ret = appender!string();
	ret.reserve(text.length);
	size_t index;
	while (index < text.length)
	{
		dchar c = decode(text, index);
		if (c == '%' && index + 2 <= text.length && text[index].isHexDigit
				&& text[index + 1].isHexDigit)
		{
			ret ~= text[index - 1 .. index + 2];
			index += 2;
		}
		else if (c.isAlphaNum || c.among!('-', '.', '_', '~', ':', '/', '?',
				'#', '[', ']', '@', '!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '='))
		{
			ret ~= c;
		}
		else
		{
			char[4] bytes;
			const len = encode(bytes, c);
			foreach (b; bytes[0 .. len])
				ret ~= encodeCharByte(b)[];
		}
	}
	return ret.data;
}

string encodeUnreserved(string text)
{
	auto ret = appender!string();
	ret.reserve(text.length);
	size_t index;
	while (index < text.length)
	{
		dchar c = decode(text, index);
		if (c.isAlphaNum || c.among!('-', '.', '_', '~'))
		{
			ret ~= c;
		}
		else
		{
			char[4] bytes;
			const len = encode(bytes, c);
			foreach (b; bytes[0 .. len])
				ret ~= encodeCharByte(b)[];
		}
	}
	return ret.data;
}

char[3] encodeCharByte(char c)
{
	return ['%', hexDigits[c >> 4], hexDigits[c & 0xF]];
}

version (unittest)
{
	private string testExpand(T)(string templateString, T variables)
	{
		const lax = expandTemplateURIString(templateString, variables, false);
		const strict = expandTemplateURIString(templateString, variables, true);
		assert(lax == strict);
		return strict;
	}
}

// test level 1 examples
@safe unittest
{
	string[string] variables = ["var" : "value", "hello" : "Hello World!"];

	assert(testExpand("https://github.com", variables) == "https://github.com");

	assert(testExpand("{var}", variables) == "value");
	assert(testExpand("{hello}", variables) == "Hello%20World%21");
}

// test level 2 examples
@safe unittest
{
	string[string] variables = [
		"var" : "value", "hello" : "Hello World!", "path" : "/foo/bar",
		"empty" : ""
	];

	assert(testExpand("{+var}", variables) == "value");
	assert(testExpand("{+hello}", variables) == "Hello%20World!");
	assert(testExpand("{+path}/here", variables) == "/foo/bar/here");
	assert(testExpand("here?ref={+path}", variables) == "here?ref=/foo/bar");

	assert(testExpand("?{var,empty}", variables) == "?value,");
	assert(testExpand("?{var,undef}", variables) == "?value");
	assert(testExpand("?{undef,var}", variables) == "?value");
}

// test level 3 examples
@safe unittest
{
	string[string] variables = [
		"var" : "value", "hello" : "Hello World!", "path" : "/foo/bar",
		"empty" : "", "x" : "1024", "y" : "768"
	];

	assert(testExpand("map?{x,y}", variables) == "map?1024,768");
	assert(testExpand("{x,hello,y}", variables) == "1024,Hello%20World%21,768");

	assert(testExpand("{+x,hello,y}", variables) == "1024,Hello%20World!,768");
	assert(testExpand("{+path,x}/here", variables) == "/foo/bar,1024/here");

	assert(testExpand("{#x,hello,y}", variables) == "#1024,Hello%20World!,768");
	assert(testExpand("{#path,x}/here", variables) == "#/foo/bar,1024/here");

	assert(testExpand("X{.var}", variables) == "X.value");
	assert(testExpand("X{.x,y}", variables) == "X.1024.768");

	assert(testExpand("{/var}", variables) == "/value");
	assert(testExpand("{/var,x}/here", variables) == "/value/1024/here");

	assert(testExpand("{;x,y}", variables) == ";x=1024;y=768");
	assert(testExpand("{;x,y,empty}", variables) == ";x=1024;y=768;empty");

	assert(testExpand("{?x,y}", variables) == "?x=1024&y=768");
	assert(testExpand("{?x,y,empty}", variables) == "?x=1024&y=768&empty=");

	assert(testExpand("?fixed=yes{&x}", variables) == "?fixed=yes&x=1024");
	assert(testExpand("{&x,y,empty}", variables) == "&x=1024&y=768&empty=");
}

// test level 4 examples
@safe unittest
{
	URIVariable[string] variables = [
		"var" : URIVariable("value"), "hello" : URIVariable("Hello World!"),
		"path" : URIVariable("/foo/bar"),
		"list" : URIVariable(["red", "green", "blue"]),
		"keys" : URIVariable([
				tuple("semi", ";"), tuple("dot", "."), tuple("comma", ",")
				]),
	];

	assert(testExpand("{var:3}", variables) == "val");
	assert(testExpand("{var:30}", variables) == "value");
	assert(testExpand("{list}", variables) == "red,green,blue");
	assert(testExpand("{list*}", variables) == "red,green,blue");
	assert(testExpand("{keys}", variables) == "semi,%3B,dot,.,comma,%2C");
	assert(testExpand("{keys*}", variables) == "semi=%3B,dot=.,comma=%2C");

	assert(testExpand("{+path:6}/here", variables) == "/foo/b/here");
	assert(testExpand("{+list}", variables) == "red,green,blue");
	assert(testExpand("{+list*}", variables) == "red,green,blue");
	assert(testExpand("{+keys}", variables) == "semi,;,dot,.,comma,,");
	assert(testExpand("{+keys*}", variables) == "semi=;,dot=.,comma=,");

	assert(testExpand("{#path:6}/here", variables) == "#/foo/b/here");
	assert(testExpand("{#list}", variables) == "#red,green,blue");
	assert(testExpand("{#list*}", variables) == "#red,green,blue");
	assert(testExpand("{#keys}", variables) == "#semi,;,dot,.,comma,,");
	assert(testExpand("{#keys*}", variables) == "#semi=;,dot=.,comma=,");

	assert(testExpand("X{.var:3}", variables) == "X.val");
	assert(testExpand("X{.list}", variables) == "X.red,green,blue");
	assert(testExpand("X{.list*}", variables) == "X.red.green.blue");
	assert(testExpand("X{.keys}", variables) == "X.semi,%3B,dot,.,comma,%2C");
	assert(testExpand("X{.keys*}", variables) == "X.semi=%3B.dot=..comma=%2C");

	assert(testExpand("{/var:1,var}", variables) == "/v/value");
	assert(testExpand("{/list}", variables) == "/red,green,blue");
	assert(testExpand("{/list*}", variables) == "/red/green/blue");
	assert(testExpand("{/list*,path:4}", variables) == "/red/green/blue/%2Ffoo");
	assert(testExpand("{/keys}", variables) == "/semi,%3B,dot,.,comma,%2C");
	assert(testExpand("{/keys*}", variables) == "/semi=%3B/dot=./comma=%2C");

	assert(testExpand("{;hello:5}", variables) == ";hello=Hello");
	assert(testExpand("{;list}", variables) == ";list=red,green,blue");
	assert(testExpand("{;list*}", variables) == ";list=red;list=green;list=blue");
	assert(testExpand("{;keys}", variables) == ";keys=semi,%3B,dot,.,comma,%2C");
	assert(testExpand("{;keys*}", variables) == ";semi=%3B;dot=.;comma=%2C");

	assert(testExpand("{?var:3}", variables) == "?var=val");
	assert(testExpand("{?list}", variables) == "?list=red,green,blue");
	assert(testExpand("{?list*}", variables) == "?list=red&list=green&list=blue");
	assert(testExpand("{?keys}", variables) == "?keys=semi,%3B,dot,.,comma,%2C");
	assert(testExpand("{?keys*}", variables) == "?semi=%3B&dot=.&comma=%2C");

	assert(testExpand("{&var:3}", variables) == "&var=val");
	assert(testExpand("{&list}", variables) == "&list=red,green,blue");
	assert(testExpand("{&list*}", variables) == "&list=red&list=green&list=blue");
	assert(testExpand("{&keys}", variables) == "&keys=semi,%3B,dot,.,comma,%2C");
	assert(testExpand("{&keys*}", variables) == "&semi=%3B&dot=.&comma=%2C");
}

// strict tests
@safe unittest
{
	import std.exception : assertThrown;

	URIVariable[string] variables = [
		"%2atest" : URIVariable("ok"), "emptylist" : URIVariable([""]),
		"emptymap" : URIVariable([tuple("foo", "")])
	];

	assertThrown!TemplateURIFormatException(expandTemplateURIString("/a{b", variables, false));
	assertThrown!TemplateURIFormatException(expandTemplateURIString("/a{b", variables, true));

	assertThrown!TemplateURIFormatException(expandTemplateURIString("/a{}", variables, false));
	assertThrown!TemplateURIFormatException(expandTemplateURIString("/a{}", variables, true));

	assertThrown!TemplateURIFormatException(expandTemplateURIString("/a{=test}", variables, false));
	assertThrown!TemplateURIFormatException(expandTemplateURIString("/a{=test}", variables, true));

	// unknown operator
	assert(expandTemplateURIString("/a{'test}", variables, false) == "/a");
	assertThrown!TemplateURIFormatException(expandTemplateURIString("/a{'test}", variables, true));
	assert(expandTemplateURIString("/a{%2test}", variables, false) == "/a");
	assertThrown!TemplateURIFormatException(expandTemplateURIString("/a{%2test}", variables, true));
	assert(expandTemplateURIString("/a{?%2atest}", variables, false) == "/a?%2atest=ok");
	assert(expandTemplateURIString("/a{?%2atest}", variables, true) == "/a?%2atest=ok");

	assert(expandTemplateURIString("/a%", variables, false) == "/a%");
	assert(expandTemplateURIString("/a%az", variables, false) == "/a%az");
	assert(expandTemplateURIString("/a%afb", variables, false) == "/a%afb");
	assertThrown!TemplateURIFormatException(expandTemplateURIString("/a%", variables, true));
	assertThrown!TemplateURIFormatException(expandTemplateURIString("/a%az", variables, true));
	assert(expandTemplateURIString("/a%afb", variables, true) == "/a%afb");

	assertThrown!TemplateURIFormatException(expandTemplateURIString("/a{?}", variables, true));
	assertThrown!TemplateURIFormatException(expandTemplateURIString("/a{?a.}", variables, true));
	assertThrown!TemplateURIFormatException(expandTemplateURIString("/a{?.a}", variables, true));
	assertThrown!TemplateURIFormatException(expandTemplateURIString("/a{?a$}", variables, true));
	assertThrown!TemplateURIFormatException(expandTemplateURIString("/a{??}", variables, true));
	assertThrown!TemplateURIFormatException(expandTemplateURIString("/a{?a:}", variables, true));
	assertThrown!TemplateURIFormatException(expandTemplateURIString("/a{?a:10000}",
			variables, true));
	assertThrown!TemplateURIFormatException(expandTemplateURIString("/a{?a:999a}", variables, true));
	assertThrown!TemplateURIFormatException(expandTemplateURIString("/a{?a:a999}", variables, true));
	assertThrown!TemplateURIFormatException(expandTemplateURIString("/a{?a:0999}", variables, true));
	assertThrown!TemplateURIFormatException(expandTemplateURIString("/a{?a,,b}", variables, true));

	assert(testExpand("{&emptylist}", variables) == "&emptylist=");
	assert(testExpand("{&emptylist*}", variables) == "&emptylist=");
	assert(testExpand("{&emptymap}", variables) == "&emptymap=foo,");
	assert(testExpand("{&emptymap*}", variables) == "&foo=");
}

// full examples
unittest
{
	URIVariable[string] variables = [
		"count" : URIVariable(["one", "two", "three"]),
		"dom" : URIVariable(["example", "com"]), "dub" : URIVariable("me/too"),
		"hello" : URIVariable("Hello World!"), "half" : URIVariable("50%"),
		"var" : URIVariable("value"), "who" : URIVariable("fred"),
		"base" : URIVariable("http://example.com/home/"),
		"path" : URIVariable("/foo/bar"),
		"list" : URIVariable(["red", "green", "blue"]),
		"keys" : URIVariable([
				tuple("semi", ";"), tuple("dot", "."), tuple("comma", ",")
				]), "v" : URIVariable("6"), "x" : URIVariable("1024"),
		"y" : URIVariable("768"), "empty" : URIVariable(""),
		"empty_keys" : URIVariable(cast(Tuple!(string, string)[])[]),
	];

	// section 3.2.1
	assert(testExpand("{count}", variables) == "one,two,three");
	assert(testExpand("{count*}", variables) == "one,two,three");
	assert(testExpand("{/count}", variables) == "/one,two,three");
	assert(testExpand("{/count*}", variables) == "/one/two/three");
	assert(testExpand("{;count}", variables) == ";count=one,two,three");
	assert(testExpand("{;count*}", variables) == ";count=one;count=two;count=three");
	assert(testExpand("{?count}", variables) == "?count=one,two,three");
	assert(testExpand("{?count*}", variables) == "?count=one&count=two&count=three");
	assert(testExpand("{&count*}", variables) == "&count=one&count=two&count=three");

	// section 3.2.2
	assert(testExpand("{var}", variables) == "value");
	assert(testExpand("{hello}", variables) == "Hello%20World%21");
	assert(testExpand("{half}", variables) == "50%25");
	assert(testExpand("O{empty}X", variables) == "OX");
	assert(testExpand("O{undef}X", variables) == "OX");
	assert(testExpand("{x,y}", variables) == "1024,768");
	assert(testExpand("{x,hello,y}", variables) == "1024,Hello%20World%21,768");
	assert(testExpand("?{x,empty}", variables) == "?1024,");
	assert(testExpand("?{x,undef}", variables) == "?1024");
	assert(testExpand("?{undef,y}", variables) == "?768");
	assert(testExpand("{var:3}", variables) == "val");
	assert(testExpand("{var:30}", variables) == "value");
	assert(testExpand("{list}", variables) == "red,green,blue");
	assert(testExpand("{list*}", variables) == "red,green,blue");
	assert(testExpand("{keys}", variables) == "semi,%3B,dot,.,comma,%2C");
	assert(testExpand("{keys*}", variables) == "semi=%3B,dot=.,comma=%2C");

	// section 3.2.3
	assert(testExpand("{+var}", variables) == "value");
	assert(testExpand("{+hello}", variables) == "Hello%20World!");
	assert(testExpand("{+half}", variables) == "50%25");

	assert(testExpand("{base}index", variables) == "http%3A%2F%2Fexample.com%2Fhome%2Findex");
	assert(testExpand("{+base}index", variables) == "http://example.com/home/index");
	assert(testExpand("O{+empty}X", variables) == "OX");
	assert(testExpand("O{+undef}X", variables) == "OX");

	assert(testExpand("{+path}/here", variables) == "/foo/bar/here");
	assert(testExpand("here?ref={+path}", variables) == "here?ref=/foo/bar");
	assert(testExpand("up{+path}{var}/here", variables) == "up/foo/barvalue/here");
	assert(testExpand("{+x,hello,y}", variables) == "1024,Hello%20World!,768");
	assert(testExpand("{+path,x}/here", variables) == "/foo/bar,1024/here");

	assert(testExpand("{+path:6}/here", variables) == "/foo/b/here");
	assert(testExpand("{+list}", variables) == "red,green,blue");
	assert(testExpand("{+list*}", variables) == "red,green,blue");
	assert(testExpand("{+keys}", variables) == "semi,;,dot,.,comma,,");
	assert(testExpand("{+keys*}", variables) == "semi=;,dot=.,comma=,");

	// section 3.2.4
	assert(testExpand("{#var}", variables) == "#value");
	assert(testExpand("{#hello}", variables) == "#Hello%20World!");
	assert(testExpand("{#half}", variables) == "#50%25");
	assert(testExpand("foo{#empty}", variables) == "foo#");
	assert(testExpand("foo{#undef}", variables) == "foo");
	assert(testExpand("{#x,hello,y}", variables) == "#1024,Hello%20World!,768");
	assert(testExpand("{#path,x}/here", variables) == "#/foo/bar,1024/here");
	assert(testExpand("{#path:6}/here", variables) == "#/foo/b/here");
	assert(testExpand("{#list}", variables) == "#red,green,blue");
	assert(testExpand("{#list*}", variables) == "#red,green,blue");
	assert(testExpand("{#keys}", variables) == "#semi,;,dot,.,comma,,");
	assert(testExpand("{#keys*}", variables) == "#semi=;,dot=.,comma=,");

	// section 3.2.5
	assert(testExpand("{.who}", variables) == ".fred");
	assert(testExpand("{.who,who}", variables) == ".fred.fred");
	assert(testExpand("{.half,who}", variables) == ".50%25.fred");
	assert(testExpand("www{.dom*}", variables) == "www.example.com");
	assert(testExpand("X{.var}", variables) == "X.value");
	assert(testExpand("X{.empty}", variables) == "X.");
	assert(testExpand("X{.undef}", variables) == "X");
	assert(testExpand("X{.var:3}", variables) == "X.val");
	assert(testExpand("X{.list}", variables) == "X.red,green,blue");
	assert(testExpand("X{.list*}", variables) == "X.red.green.blue");
	assert(testExpand("X{.keys}", variables) == "X.semi,%3B,dot,.,comma,%2C");
	assert(testExpand("X{.keys*}", variables) == "X.semi=%3B.dot=..comma=%2C");
	assert(testExpand("X{.empty_keys}", variables) == "X");
	assert(testExpand("X{.empty_keys*}", variables) == "X");

	// section 3.2.6
	assert(testExpand("{/who}", variables) == "/fred");
	assert(testExpand("{/who,who}", variables) == "/fred/fred");
	assert(testExpand("{/half,who}", variables) == "/50%25/fred");
	assert(testExpand("{/who,dub}", variables) == "/fred/me%2Ftoo");
	assert(testExpand("{/var}", variables) == "/value");
	assert(testExpand("{/var,empty}", variables) == "/value/");
	assert(testExpand("{/var,undef}", variables) == "/value");
	assert(testExpand("{/var,x}/here", variables) == "/value/1024/here");
	assert(testExpand("{/var:1,var}", variables) == "/v/value");
	assert(testExpand("{/list}", variables) == "/red,green,blue");
	assert(testExpand("{/list*}", variables) == "/red/green/blue");
	assert(testExpand("{/list*,path:4}", variables) == "/red/green/blue/%2Ffoo");
	assert(testExpand("{/keys}", variables) == "/semi,%3B,dot,.,comma,%2C");
	assert(testExpand("{/keys*}", variables) == "/semi=%3B/dot=./comma=%2C");

	// section 3.2.7
	assert(testExpand("{;who}", variables) == ";who=fred");
	assert(testExpand("{;half}", variables) == ";half=50%25");
	assert(testExpand("{;empty}", variables) == ";empty");
	assert(testExpand("{;v,empty,who}", variables) == ";v=6;empty;who=fred");
	assert(testExpand("{;v,bar,who}", variables) == ";v=6;who=fred");
	assert(testExpand("{;x,y}", variables) == ";x=1024;y=768");
	assert(testExpand("{;x,y,empty}", variables) == ";x=1024;y=768;empty");
	assert(testExpand("{;x,y,undef}", variables) == ";x=1024;y=768");
	assert(testExpand("{;hello:5}", variables) == ";hello=Hello");
	assert(testExpand("{;list}", variables) == ";list=red,green,blue");
	assert(testExpand("{;list*}", variables) == ";list=red;list=green;list=blue");
	assert(testExpand("{;keys}", variables) == ";keys=semi,%3B,dot,.,comma,%2C");
	assert(testExpand("{;keys*}", variables) == ";semi=%3B;dot=.;comma=%2C");

	// section 3.2.8
	assert(testExpand("{?who}", variables) == "?who=fred");
	assert(testExpand("{?half}", variables) == "?half=50%25");
	assert(testExpand("{?x,y}", variables) == "?x=1024&y=768");
	assert(testExpand("{?x,y,empty}", variables) == "?x=1024&y=768&empty=");
	assert(testExpand("{?x,y,undef}", variables) == "?x=1024&y=768");
	assert(testExpand("{?var:3}", variables) == "?var=val");
	assert(testExpand("{?list}", variables) == "?list=red,green,blue");
	assert(testExpand("{?list*}", variables) == "?list=red&list=green&list=blue");
	assert(testExpand("{?keys}", variables) == "?keys=semi,%3B,dot,.,comma,%2C");
	assert(testExpand("{?keys*}", variables) == "?semi=%3B&dot=.&comma=%2C");

	// section 3.2.9
	assert(testExpand("{&who}", variables) == "&who=fred");
	assert(testExpand("{&half}", variables) == "&half=50%25");
	assert(testExpand("?fixed=yes{&x}", variables) == "?fixed=yes&x=1024");
	assert(testExpand("{&x,y,empty}", variables) == "&x=1024&y=768&empty=");
	assert(testExpand("{&x,y,undef}", variables) == "&x=1024&y=768");

	assert(testExpand("{&var:3}", variables) == "&var=val");
	assert(testExpand("{&list}", variables) == "&list=red,green,blue");
	assert(testExpand("{&list*}", variables) == "&list=red&list=green&list=blue");
	assert(testExpand("{&keys}", variables) == "&keys=semi,%3B,dot,.,comma,%2C");
	assert(testExpand("{&keys*}", variables) == "&semi=%3B&dot=.&comma=%2C");
}
