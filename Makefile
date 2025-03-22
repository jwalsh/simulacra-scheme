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

# Run tests (placeholder for future test implementation)
test:
	@echo "No tests implemented yet. See GitHub issue #6"
	@echo "Future implementation will use SRFI-64 test framework"

# Clean any generated files
clean:
	rm -f *.log *.out generated-diagrams.md
	find . -name "*.go" -delete

# Set up environment (placeholder)
setup:
	@echo "Checking Guile installation..."
	@$(SCHEME) --version

.PHONY: all run-example run-demo run-hyperreality run-disneyland run-watergate generate-diagrams test clean setup
