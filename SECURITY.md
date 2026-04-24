# Security Policy

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| main    | :white_check_mark: |
| dev     | :white_check_mark: |

## Reporting a Vulnerability

If you discover a security vulnerability in this project, please report it responsibly.

**Do NOT open a public issue.**

Instead, please use [GitHub Security Advisories](https://github.com/derekdogg/Crafting-Interpreters/security/advisories/new) to privately report the vulnerability.

Include:
- A description of the vulnerability
- Steps to reproduce
- Potential impact
- Suggested fix (if any)

## Scope

This is an educational bytecode interpreter based on [Crafting Interpreters](https://craftinginterpreters.com/) by Bob Nystrom. It is not intended for production use. However, we still take security seriously, especially:

- Memory safety issues in the VM
- Buffer overflows in the scanner/parser/compiler
- Denial of service via crafted `.lox` input files
- Arbitrary code execution
