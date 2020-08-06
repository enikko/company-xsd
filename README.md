# company-xsd
Company-xsd is a backend for company-mode that provides context
dependent auto-completions based on the xsd-schemas associated with
the xml-file.

## Requirements
Company-xsd requires a modern Emacs version (tested with 25.2.2 but
anything later than 25.1 should work) and company-mode.

## Usage
Like any company-mode backend, the completion purely uses
company-mode as frontend with one exception: Force a new guess of
schema files. The backend will try to find the schema file to use as
soon as the an xml-file is opened. However, if the schema file is
specified in the xml-file after it is opened or if it's changed, then
it becomes necessary to tell the backend to change schema file. This
is done by calling the interactive function `company-xsd-init-buffer`
(if called from code it must be called interactively).

## Installation
The most simple and common usage is to add the following to `.emacs`

```elisp
(require 'company-xsd)
(add-to-list 'company-backends 'company-xsd-backend)
```

This will enable the backend as long as the schemas are specified correctly
in the root element in the file. If this is not enought there are a few options
for manually configuring the backend:

### Manual schema specification
In the case that the schema files are not specified correctly, for some reason,
then it is possible to set the manually through the variable `company-xsd-schemas`.
The variable is a sequence of `(namespace . location)` pairs, where:

* `namespace` is either nil, i.e. uses the global namespace, or a string
  that is the namespace.
* `location` is either an URI to the schema or a relative path to the
  schema.

Note that if `company-xsd-schemas` is set, then the backend will not
guess which schema files to use.

### Inserting deducible information
Many xsd-schemas have required attributes and sub-tags that must be
present in a tag. The auto-completion tries to insert as much as
possible when it can (and when it is implemented). Currently, this is
an on-off feature set by variable `company-xsd-insert-deducible` that
defaults to on.

### Custom guess schema function
If for some reason, the xml-file specifies a schema in a non-standard
way and it's not reasonable to specify the schema file with the
`company-xsd-schemas` variable, then the `company-xsd-guess-schemas`
function can be overriden. The function takes no arguments and should
return a sequence of `(namespace . location)` pairs, the same format
as `company-xsd-guess-schemas`.

### Custom uri fetcher
The company backend uses a xsd compiler (`xsd.el`) that is a part of
this package. Because the xsd schemas can contain sub-schemas
specified by an URI, the compiler must be able to fetch those
sub-schemas. However, if one does not want the editor to redirect
these fetches (for example, when working with local schemas that will
be published on a different server than the xml files), then one can
redefined the function `xsd-uri-fetch`. This function takes a string
argument that is the URI and it is expected to create a new buffer
containing the xsd-schema. Furthermore, it is responsible for that the
buffer is closed properly (unless it's acceptable with background buffers).
