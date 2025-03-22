# Makefile for Simulacra-Scheme project

# Using Guile Scheme as specified
SCHEME = guile

# Default target
all: run-example

# Run the main example
run-example:
	$(SCHEME) -l example.scm -c "(run-social-media-simulation)"

# Run the basic simulacra demo
run-demo:
	$(SCHEME) -l simulacra.scm -c "(demo-progression)"

# Run hyperreality demo
run-hyperreality:
	$(SCHEME) -l simulacra.scm -c "(create-hyperreality)"

# Run the Disneyland effect demo
run-disneyland:
	$(SCHEME) -l simulacra.scm -c "(disneyland-effect)"

# Run the Watergate effect demo
run-watergate:
	$(SCHEME) -l simulacra.scm -c "(watergate-effect)"

# Generate Mermaid diagrams
generate-diagrams:
	$(SCHEME) -l mermaid-generator.scm

# Clean any generated files
clean:
	rm -f *.log *.out generated-diagrams.md

.PHONY: all run-example run-demo run-hyperreality run-disneyland run-watergate generate-diagrams clean
