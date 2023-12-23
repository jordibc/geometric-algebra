# Mantainers

This information is meant for project mantainers. You don't need if
you just want to use the library.


## Adding documentation

We would like to add more documentation, following the ideas in
http://jacobian.org/writing/what-to-write/


## Testing

You can run some tests with:

```sh
clojure -T:build test
```


## Building a JAR

To run the project's continuous integration (CI) pipeline and build a
JAR file:

```sh
clojure -T:build ci
```

This will produce an updated `pom.xml` file with synchronized
dependencies inside the `META-INF` directory inside `target/classes`
and the JAR in `target`. You can update the version (and SCM tag)
information in generated `pom.xml` by updating `build.clj`.

To install it locally (which requires the `ci` task be run first):

```sh
clojure -T:build install
```


## Deployment

To deploy this library to Clojars, run the `ci` task first, then set
environment variables `CLOJARS_USERNAME` and `CLOJARS_PASSWORD`, and
run:

```sh
clojure -T:build deploy
```

The library will be deployed to
[net.clojars.jordibc/geometric-algebra](https://clojars.org/net.clojars.jordibc/geometric-algebra)
on [clojars.org](https://clojars.org/) by default.
