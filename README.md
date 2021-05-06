# uritemplate

An IETF [RFC 6570](https://tools.ietf.org/html/rfc6570) URI Template expansion implementation for the [D programming language](https://dlang.org).

URI templates can be used for example in REST APIs to indicate related APIs to call, while being able to modify and extend URIs for future updates. This is for example used by the GitHub API.

This library can be used for

* Expansion
* Strict validation
* `@safe` code

It's using idiomatic D code and the standard phobos style guide. Samples from the RFC are included as unittests and documented as such if you wish to view the source code. As the "Expansion" part of the RFC is fully implemented, no extensions to the code are currently planned. `@nogc` / buffer based outputs may be added if desired.

**Example**

```d
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
```

The code has 100% test coverage and is well documented. Refer to the auto-generated [dpldocs page](https://uritemplate.dpldocs.info) for documentation or use an IDE plugin for in-editor documentation. It was originally intended for the dub registry to properly use the GitHub API, but was separated to its own package due to the high conformity and usability.

This project is released under the unlicense, a public domain license. Feel free to use this code however you want without any restrictions. See LICENSE.md for details.
