# Resources ttl generator

Derives a ttl from a mu-cl-resources specification

## Example usage

```
    # Generate the sources
    docker run \
           -v `pwd`/config/resources:/config \
           -v `pwd`/doc/:/config/output \
           madnificent/cl-resources-ttl-generator
    
    # Copy from the terminal, or open the generated json file
    cat /tmp/model.ttl
```

The output contains the model.  `/tmp/openapi.json` will contain the generated model as well.  You can upload this to http://visualdataweb.de/webvowl/ with the menu at the bottom.

If the output contains an error, rather than a Lisp model, there's likely some missing elements in the domain.lisp and/or repository.lisp.  The plantuml generator provides better error logging in this regard.
