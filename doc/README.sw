== sass

=== Description

This egg is a wrapper for [[http://libsass.org/|libsass]], the C/C++
library version of the Sass CSS preprocessor. As of version 0.1, it
is just a literal translation of the C API.


[[toc:]]


=== Authors

Matt Gushee <matt@gushee.net>


=== Requirements

[[foreigners]], [[http://libsass.org/|libsass]]


=== High-level API

==== Usage

    (use sass)

==== Procedures

<procedure>(compile-file      FILENAME  #!key kwargs)</procedure>
<procedure>(compile-string    STRING    #!key kwargs)</procedure>
<procedure>(compile-from-port PORT      #!key kwargs)</procedure>

{{compile-file}}, {{compile-string}}, and {{compile-from-port}} accept the
following keyword arguments:


; output : [string/port - default {{'stdout}}]  The destination for the output. May be a filename, a port, or the symbol {{'stdout}}.
; precision : [integer - default {{#f}}]  The precision, in number of digits, for decimal numbers.
; output-style : [symbol - default {{#f}}]  One of {{'nested}}, {{'expanded}}, {{'compact}}, or {{'compressed}}. This option controls the formatting of the output CSS.
; source-comments : [boolean - default {{'undefined}}]   This option controls whether comments are placed in the output to identify the line number in the source file where each selector is defined.
; source-map-embed : [boolean - default {{'undefined}}]  Whether to embed {{sourceMappingUrl}} as data URI.
; source-map-contents : [boolean - default {{'undefined}}]  Whether to embed include contents in sourcemap files. 
; omit-source-map-url : [boolean - default {{'undefined}}]  Whether to disable {{sourceMappingUrl}} in CSS output. 
; is-indented-syntax-src : [boolean - default {{'undefined}}]  Assume source file format is SASS rather than SCSS.
; indent : [string - default {{#f}}]  The string to use for indentation.
; linefeed : [string - default {{#f}}]  The string to use for linefeeds.
; input-path : [string - default {{#f}}]  Input path for source map generation.
; output-path : [string - default {{#f}}]  Output path for source map generation.
; plugin-path : [string - default {{#f}}]  A list of paths - semicolon-separated on Windows, colon-separated otherwise.
; include-path : [string - default {{#f}}]  A list of paths - semicolon-separated on Windows, colon-separated otherwise.
; source-map-file : [string - default {{#f}}]  Path of a source map file.  If non-empty, enables source map generation.
; source-map-root : [string - default {{#f}}]  Root directory inserted in source maps.
; c-headers : [default {{#f}}]  No documentation available.
; c-importers : [default {{#f}}]  Overload imports. 
; c-functions : [default {{#f}}]  List of custom functions callable from SCSS code.


=== Low-level API

The following documents the entire {{sass-context}} API, which is the primary
public interface to the C library. The {{sass-values}} and {{sass-functions}}
modules are currently undocumented. 

==== Usage

    (use sass-context)


==== TYPES

All the following are foreign types, and can only be created using the
appropriate API functions.

<type>OPTIONS</type>

Represents a key-value structure of options. Use {{make-options}} or
{{get-options}} to create.

<type>INPUT-CONTEXT</type>

A record type wrapping either of the foreign pointer types {{Sass_File_Context}} or {{Sass_Data_Context}}, which represent, respectively, the input file or
string. Use {{make-file-context}} or {{make-data-context}} to create.

<type>CONTEXT</type>

Represents the output. Use {{get-context}} to create.

<type>SASS-COMPILER</type>

Represents the compiler. Use {{make-compiler}} to create.



==== PROCEDURES

<procedure>(make-options)</procedure>

Returns an OPTIONS object.

<procedure>(make-file-context FILENAME)</procedure>

Returns an INPUT-CONTEXT object with type tag 'file.

<procedure>(make-data-context INPUT-STRING)</procedure>

Returns an INPUT-CONTEXT object with type tag 'data.

<procedure>(compile-input-context INPUT-CONTEXT)</procedure>

The usual method to invoke compilation of a stylesheet.

<procedure>(make-compiler INPUT-CONTEXT)</procedure>

Returns a SASS-COMPILER object.

<procedure>(compiler-parse SASS-COMPILER)</procedure>

<procedure>(compiler-execute SASS-COMPILER)</procedure>

<procedure>(delete-compiler SASS-COMPILER)</procedure>

<procedure>(delete-input-context INPUT-CONTEXT)</procedure>

<procedure>(get-context INPUT-CONTEXT)</procedure>

Returns a CONTEXT object.

<procedure>(get-options CONTEXT/INPUT-CONTEXT)</procedure>

<procedure>(set-options! INPUT-CONTEXT OPTIONS)</procedure>

<procedure>(opt-precision SASS-OPTIONS)</procedure>

<procedure>(opt-output-style SASS-OPTIONS)</procedure>

<procedure>(opt-source-comments SASS-OPTIONS)</procedure>

<procedure>(opt-source-map-embed SASS-OPTIONS)</procedure>

<procedure>(opt-source-map-contents SASS-OPTIONS)</procedure>

<procedure>(opt-omit-source-map-url SASS-OPTIONS)</procedure>

<procedure>(opt-is-indented-syntax-src SASS-OPTIONS)</procedure>

<procedure>(opt-indent SASS-OPTIONS)</procedure>

<procedure>(opt-linefeed SASS-OPTIONS)</procedure>

<procedure>(opt-input-path SASS-OPTIONS)</procedure>

<procedure>(opt-output-path SASS-OPTIONS)</procedure>

<procedure>(opt-plugin-path SASS-OPTIONS)</procedure>

<procedure>(opt-include-path SASS-OPTIONS)</procedure>

<procedure>(opt-source-map-file SASS-OPTIONS)</procedure>

<procedure>(opt-source-map-root SASS-OPTIONS)</procedure>

<procedure>(opt-c-headers SASS-OPTIONS)</procedure>

<procedure>(opt-c-importers SASS-OPTIONS)</procedure>

<procedure>(opt-c-functions SASS-OPTIONS)</procedure>

<procedure>(opt-precision-set! SASS-OPTIONS INTEGER)</procedure>

<procedure>(opt-output-style-set! SASS-OPTIONS OUTPUT-STYLE)</procedure>

<procedure>(opt-source-comments-set! SASS-OPTIONS BOOLEAN)</procedure>

<procedure>(opt-source-map-embed-set! SASS-OPTIONS BOOLEAN)</procedure>

<procedure>(opt-source-map-contents-set! SASS-OPTIONS BOOLEAN)</procedure>

<procedure>(opt-omit-source-map-url-set! SASS-OPTIONS BOOLEAN)</procedure>

<procedure>(opt-is-indented-syntax-src-set! SASS-OPTIONS BOOLEAN)</procedure>

<procedure>(opt-indent-set! SASS-OPTIONS STRING)</procedure>

<procedure>(opt-linefeed-set! SASS-OPTIONS STRING)</procedure>

<procedure>(opt-input-path-set! SASS-OPTIONS STRING)</procedure>

<procedure>(opt-output-path-set! SASS-OPTIONS STRING)</procedure>

<procedure>(opt-plugin-path-set! SASS-OPTIONS STRING)</procedure>

<procedure>(opt-include-path-set! SASS-OPTIONS STRING)</procedure>

<procedure>(opt-source-map-file-set! SASS-OPTIONS STRING)</procedure>

<procedure>(opt-source-map-root-set! SASS-OPTIONS STRING)</procedure>

<procedure>(opt-c-headers-set! SASS-OPTIONS IMPORTER-LIST)</procedure>

<procedure>(opt-c-importers-set! SASS-OPTIONS IMPORTER-LIST)</procedure>

<procedure>(opt-c-functions-set! SASS-OPTIONS FUNCTION-LIST)</procedure>

<procedure>(context-get-output-string SASS-CONTEXT)</procedure>

<procedure>(context-get-error-status SASS-CONTEXT)</procedure>

<procedure>(context-get-error-json SASS-CONTEXT)</procedure>

<procedure>(context-get-error-text SASS-CONTEXT)</procedure>

<procedure>(context-get-error-message SASS-CONTEXT)</procedure>

<procedure>(context-get-error-file SASS-CONTEXT)</procedure>

<procedure>(context-get-error-src SASS-CONTEXT)</procedure>

<procedure>(context-get-error-line SASS-CONTEXT)</procedure>

<procedure>(context-get-error-column SASS-CONTEXT)</procedure>

<procedure>(context-get-source-map-string SASS-CONTEXT)</procedure>

<procedure>(context-get-included-files SASS-CONTEXT)</procedure>

<procedure>(context-get-included-files-size SASS-CONTEXT)</procedure>

<procedure>(context-take-error-json SASS-CONTEXT)</procedure>

<procedure>(context-take-error-text SASS-CONTEXT)</procedure>

<procedure>(context-take-error-message SASS-CONTEXT)</procedure>

<procedure>(context-take-error-file SASS-CONTEXT)</procedure>

<procedure>(context-take-output-string SASS-CONTEXT)</procedure>

<procedure>(context-take-source-map-string SASS-CONTEXT)</procedure>

<procedure>(context-take-included-files SASS-CONTEXT)</procedure>

<procedure>(compiler-get-state SASS-COMPILER)</procedure>

<procedure>(compiler-get-context SASS-COMPILER)</procedure>

<procedure>(compiler-get-import-stack-size SASS-COMPILER)</procedure>

<procedure>(compiler-get-last-import SASS-COMPILER)</procedure>

<procedure>(compiler-get-import-entry SASS-COMPILER INTEGER)</procedure>

<procedure>(option-push-plugin-path SASS-OPTIONS STRING)</procedure>

<procedure>(option-push-include-path SASS-OPTIONS STRING)</procedure>


=== Examples

==== simple.scm

Here is a simple file compiler using the {{sass-context}} API. This example
is adapted from [[https://github.com/sass/libsass/wiki/API-Sass-Context-Example]].

    (use (prefix sass sass:))
    
    (define (main)
      (let* ((args (command-line-arguments))
             (infile
               (if (null? args)
                 "styles.scss"
                 (car args)))
             (file-ctx (sass:make-file-context infile))
             (ctx (sass:get-context file-ctx))
             (ctx-opts (sass:get-options ctx)))
    
        (sass:opt-precision-set! ctx-opts 10)
    
        (let ((status (sass:compile-input-context file-ctx)))
          (if (zero? status)
            (display (sass:output-string ctx))
            (error (sass:error-message ctx))))
    
        (sass:delete-input-context file-ctx)))
    
    (main)

To use this example:

    csc simple.scm
    echo "foo { margin: 21px * 2; }" > foo.scss
    ./simple foo.scss => "foo { margin: 42px }"


=== In case of bugs

If you have a GitHub account, please use the 
[[https://github.com/mgushee/chicken-sass/issues|GitHub issue tracker]] -- likewise
for any technical questions or suggestions you may have (other than how-to
type questions). If you are unable to do this, the chicken-users mailing
list will also work.

=== License

Copyright (c) 2015, Matthew C. Gushee
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:


    Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.

    Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
    
    Neither the name of the author nor the names of its contributors may be
    used to endorse or promote products derived from this software without
    specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.


=== Repo

[[https://github.com/mgushee/chicken-sass]]


=== Version History

; 0.2 : [June 3, 2015] Added high-level {{compile-*}} procedures; merged {{FILE-CONTEXT}} and {{DATA-CONTEXT}} types into new {{INPUT-CONTEXT}} type; made {{sass-context}} API names more concise; moved {{csass}} program into new {{csass-utils}} egg.

; 0.1 : Initial release.
