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


=== Installation Notes

This egg includes a command-line Sass compiler called {{csass}}. However,
since at this point it is simply [[https://github.com/sass/sassc|sassc]]
rewritten in Scheme, so it is perhaps redundant and thus will not be installed by default. If you wish to install {{csass}}, pass the {{-D with-csass}}
option to {{chicken-install}}. E.g.,

    chicken-install -D with-csass sass 


=== API

The following documents the entire {{sass-context}} API, which is the primary
public interface. The {{sass-values}} and {{sass-functions}} modules are
currently undocumented. 

==== TYPES

All the following are foreign types, and can only be created using the
appropriate API functions.

<type>SASS-OPTIONS</type>

A key-value structure containing various options that control the
{{*-CONTEXT}} objects. Use {{make-options}} to create. May also be
obtained using {{context-get-options}}, {{file-context-get-options}},
or {{data-context-get-options}}.

<type>SASS-FILE-CONTEXT</type>

Represents the input file. Use {{make-file-context}} to create.

<type>SASS-DATA-CONTEXT</type>

Represents an input string. Use {{make-data-context}} to create.

<type>SASS-CONTEXT</type>

Represents the output. Use {{file-context-get-context}} or
{{data-context-get-context}} to create.

<type>SASS-COMPILER</type>

Represents the compiler. Use {{make-file-compiler}} or
{{make-data-compiler}} to create.


==== PROCEDURES

<procedure>(make-options)</procedure>

Returns a SASS-OPTIONS object.

<procedure>(make-file-context FILENAME)</procedure>

Returns a SASS-FILE-CONTEXT object.

<procedure>(make-data-context INPUT-STRING)</procedure>

Returns a SASS-DATA-CONTEXT object.

<procedure>(compile-file-context SASS-FILE-CONTEXT)</procedure>

<procedure>(compile-data-context SASS-DATA-CONTEXT)</procedure>

<procedure>(make-file-compiler SASS-FILE-CONTEXT)</procedure>

Returns a SASS-COMPILER object.

<procedure>(make-data-compiler SASS-DATA-CONTEXT)</procedure>

Returns a SASS-COMPILER object.

<procedure>(compiler-parse SASS-COMPILER)</procedure>

<procedure>(compiler-execute SASS-COMPILER)</procedure>

<procedure>(delete-compiler SASS-COMPILER)</procedure>

<procedure>(delete-file-context SASS-FILE-CONTEXT)</procedure>

<procedure>(delete-data-context SASS-DATA-CONTEXT)</procedure>

<procedure>(file-context-get-context SASS-FILE-CONTEXT)</procedure>

Returns a SASS-CONTEXT object.

<procedure>(data-context-get-context SASS-DATA-CONTEXT)</procedure>

Returns a SASS-CONTEXT object.

<procedure>(context-get-options SASS-CONTEXT)</procedure>

<procedure>(file-context-get-options SASS-FILE-CONTEXT)</procedure>

<procedure>(data-context-get-options SASS-DATA-CONTEXT)</procedure>

<procedure>(file-context-set-options! SASS-FILE-CONTEXT SASS-OPTIONS)</procedure>

<procedure>(data-context-set-options! SASS-DATA-CONTEXT SASS-OPTIONS)</procedure>

<procedure>(option-get-precision SASS-OPTIONS)</procedure>

<procedure>(option-get-output-style SASS-OPTIONS)</procedure>

<procedure>(option-get-source-comments SASS-OPTIONS)</procedure>

<procedure>(option-get-source-map-embed SASS-OPTIONS)</procedure>

<procedure>(option-get-source-map-contents SASS-OPTIONS)</procedure>

<procedure>(option-get-omit-source-map-url SASS-OPTIONS)</procedure>

<procedure>(option-get-is-indented-syntax-src SASS-OPTIONS)</procedure>

<procedure>(option-get-indent SASS-OPTIONS)</procedure>

<procedure>(option-get-linefeed SASS-OPTIONS)</procedure>

<procedure>(option-get-input-path SASS-OPTIONS)</procedure>

<procedure>(option-get-output-path SASS-OPTIONS)</procedure>

<procedure>(option-get-plugin-path SASS-OPTIONS)</procedure>

<procedure>(option-get-include-path SASS-OPTIONS)</procedure>

<procedure>(option-get-source-map-file SASS-OPTIONS)</procedure>

<procedure>(option-get-source-map-root SASS-OPTIONS)</procedure>

<procedure>(option-get-c-headers SASS-OPTIONS)</procedure>

<procedure>(option-get-c-importers SASS-OPTIONS)</procedure>

<procedure>(option-get-c-functions SASS-OPTIONS)</procedure>

<procedure>(option-set-precision! SASS-OPTIONS INTEGER)</procedure>

<procedure>(option-set-output-style! SASS-OPTIONS OUTPUT-STYLE)</procedure>

<procedure>(option-set-source-comments! SASS-OPTIONS BOOLEAN)</procedure>

<procedure>(option-set-source-map-embed! SASS-OPTIONS BOOLEAN)</procedure>

<procedure>(option-set-source-map-contents! SASS-OPTIONS BOOLEAN)</procedure>

<procedure>(option-set-omit-source-map-url! SASS-OPTIONS BOOLEAN)</procedure>

<procedure>(option-set-is-indented-syntax-src! SASS-OPTIONS BOOLEAN)</procedure>

<procedure>(option-set-indent! SASS-OPTIONS STRING)</procedure>

<procedure>(option-set-linefeed! SASS-OPTIONS STRING)</procedure>

<procedure>(option-set-input-path! SASS-OPTIONS STRING)</procedure>

<procedure>(option-set-output-path! SASS-OPTIONS STRING)</procedure>

<procedure>(option-set-plugin-path! SASS-OPTIONS STRING)</procedure>

<procedure>(option-set-include-path! SASS-OPTIONS STRING)</procedure>

<procedure>(option-set-source-map-file! SASS-OPTIONS STRING)</procedure>

<procedure>(option-set-source-map-root! SASS-OPTIONS STRING)</procedure>

<procedure>(option-set-c-headers! SASS-OPTIONS IMPORTER-LIST)</procedure>

<procedure>(option-set-c-importers! SASS-OPTIONS IMPORTER-LIST)</procedure>

<procedure>(option-set-c-functions! SASS-OPTIONS FUNCTION-LIST)</procedure>

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
             (ctx (sass:file-context-get-context file-ctx))
             (ctx-opts (sass:context-get-options ctx)))
    
        (sass:option-set-precision! ctx-opts 10)
    
        (let ((status (sass:compile-file-context file-ctx)))
          (if (zero? status)
            (display (sass:context-get-output-string ctx))
            (error (sass:context-get-error-message ctx))))
    
        (sass:delete-file-context file-ctx)))
    
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

;0.1:       Initial release.
