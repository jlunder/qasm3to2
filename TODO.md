# qasm3to2 TODO

## Goal

To translate whatever possible from QASM3 to QASM2, especially what I need to
test the viability of math macros in QASM3.

## Overall Structure

The actual translation process is broken down into distinct stages:

1. Parse QASM3 source into an AST
2. Perform enough semantic analysis to check consistency of source
3. Generate an IR representing what QASM2 supports + the things we can
   simulate at compile time
4. Reduce that (through compile-time processing) to a QASM2 AST
5. Emit QASM2

During this process we want to avoid losing information (like comments and
symbol names) which would make the conversion less human-recognizable.

## Testing Strategy

### QASM3 Parser

The QASM3 parser is largely tested by an automated QuickCheck test, which
generates random ASTs, pretty-prints them, and then parses the generated text,
asserting that it should generate an equivalent AST (ignoring comments,
whitespace differences, and parentheses which don't otherwise affect the
program structure). This necessitates a functional pretty-printer for QASM3
which otherwise might not be needed, but the design of that and the QuickCheck
generator were very straightforward and this provides a level of fuzzing which
would be difficult to achieve otherwise.

If challenging parse cases are discovered, specific automated test cases can
be added to verify that specific text produces the desired AST.

### Conversion

A second automated QuickCheck test generates a subset of QASM3 programs which
can be converted to QASM2. This generator is parameterized on characteristics
which are visible in the output, and various properties are checked this way.

Additionally, a suite of input/output pairs are provided in the test-data
folder. The tests iterate these pairs to check that conversion generates the
expected output.

## Detailed Design

### Parsing

The QASM3 parser is implemented using Alex and Happy, in the Qasm3Lexer.x and
Qasm3Parser.y files. The translation from the QASM3 ANTLR grammars is fairly
straightforward, with some minor deviations noted in the grammar files.

The output is an AST.

### Semantic Analysis

- What are types on input/output?

- Analyze types
- Check validity

### Generate IR

- What are types on input/output?

- Check compatibility
- Fold constants

### Lower To QASM2

- What are types on input/output?

- Expand loops/conditionals
- Translate to QASM2 tags

### Emitting QASM2

- What are types on input/output?

- QASM2 AST pretty-print

## Tasks

### Refactoring

- Rework AST nodes to be one uniform type with tag and context annotations

### Implemement MVP QASM2 generation

- Plan type analysis
- Tests: Adapt Qasm3Abitrary to generate convertible QASM3 programs
  (Qasm3ToQasm2Abitrary?)
- Tests: Implement "test-data" scanner to read/convert/check samples

### Improve QASM3 Parser

- Improve generated program coverage (Qasm3Abitrary.hs)
  - arbitraryStatementContentNode
  - arbitraryScalarOrArrayTypeNode
  - arbitraryExpressionNode
  - arbitraryArgumentExpression
  - arbitraryDeclarationExpressionNode
  - arbitraryMeasureExpressionNode
  - arbitraryArrayLiteralNode
  - arbitraryArrayLiteralElementNode
  - arbitraryIndexedIdentifierNode
  - arbitraryGateModifierNode
  - arbitraryScalarTypeNode
  - arbitraryQubitTypeNode
  - arbitraryArrayTypeNode
  - arbitraryArrayReferenceTypeNode
  - arbitraryDefcalTargetNode
  - arbitraryDefcalArgumentDefinitionNode
  - arbitraryDefcalOperandNode
  - arbitraryGateOperandNode
  - arbitraryExternArgumentNode
  - arbitraryArgumentDefinitionNode
- Improve lexing of nested cal blocks

### Get basic QASM3 parser working

- Done!

