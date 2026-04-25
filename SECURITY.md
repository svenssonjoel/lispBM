
# Security Policy

LispBM releases are given git tags in the a.b.c version format.  Only
the commit with the numbered tag has been subjected to the full
test-suite and considered a stable point. Between releases many git
commits can occur where the version number stored in lbm_version.h
does not change, these are to be considered experimental!

## Supported Versions

Only the latest version of lispbm is to be considered
supported. Security patches will be worked into the main branch of the
LispBM repository and will then make it into the next numbered release
of LispBM.

## Reporting Vulnerabilities

Please report security vulnerabilities to bo.joel.svensson@gmail.com
rather than opening a public issue.

Include a description of the vulnerability and the steps needed to
reproduce it.  We are very grateful for your contributions.

## Scope and Limitations

LispBM is meant to be integrated into a larger application and provide
that application with a scripting layer. The security properties of
the system as a whole is the responsibility of the
integrator. Integrators are responsible for authentication of code and
for configuration of isolation features (such as TrustZone) where
relevant.

There is a library of cryptography functions available as extensions
to the core LispBM. We hope these help users towards the requirements
of the Cyber Resilience Act or other similar regional cybersecurity
standards.

## Conclusion

LispBM is free and open source and we do our best to create a very
well tested, stable and secure base system. Contributions are very
welcome. If there is a desire to use LispBM in a way that requires
additional safety features, we are open to helping out with such work
for compensation.