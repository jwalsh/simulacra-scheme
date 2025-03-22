#!/bin/bash
# Setup script for simulacra-scheme repository
# Run from the repository directory

# Create initial GitHub repository
echo "Creating GitHub repository..."
gh repo create simulacra-scheme --public --source=. --push

# Update repository description
echo "Updating repository description..."
gh repo edit simulacra-scheme --description "Explore Jean Baudrillard's 'Simulacra and Simulation' through functional programming in Guile Scheme. This project implements hyperreality concepts, simulation stages, and the precession of simulacra as executable code with interactive demonstrations and Mermaid visualizations."

# Add topics to the repository
echo "Adding repository topics..."
gh repo edit simulacra-scheme --add-topic guile-scheme,baudrillard,simulacra,hyperreality,simulation,postmodernism,philosophy,functional-programming,mermaid

# Create GitHub issues
echo "Creating GitHub issues..."

# Issue 1
gh issue create --repo jwalsh/simulacra-scheme --title "Add Interactive REPL for Exploring Simulation Stages" --body "Create a REPL interface that allows users to interactively explore the four stages of simulacra by inputting examples and seeing how they progress through each simulation phase. Should include helpful prompts and explanations that connect the code back to Baudrillard's philosophy."

# Issue 2
gh issue create --repo jwalsh/simulacra-scheme --title "Enhance Mermaid Diagrams with Interactive Features" --body "Improve the Mermaid diagram generator to create more sophisticated visualizations of hyperreality formation. Add interactive elements that allow users to manipulate parameters and see how changes affect the progression of simulacra."

# Issue 3
gh issue create --repo jwalsh/simulacra-scheme --title "Implement Modern Social Media Simulation Example" --body "Expand the social media simulation to model how modern platforms like TikTok, Instagram, and Twitter create hyperreality through algorithm-driven content delivery, filter bubbles, and viral information cascades. Incorporate Baudrillard's theories on how media consumption shapes reality perception."

# Issue 4
gh issue create --repo jwalsh/simulacra-scheme --title "Add Comprehensive Test Suite" --body "Develop a test suite using SRFI-64 or another Guile-compatible testing framework. Include tests for all core functions, edge cases in simulation progression, and validation of the realistic behavior of social media example. Ensure tests are cross-platform compatible."

# Issue 5
gh issue create --repo jwalsh/simulacra-scheme --title "Expand Documentation with Educational Examples" --body "Create comprehensive documentation that explains how each part of the codebase relates to Baudrillard's philosophical concepts. Include annotated examples, an educational guide, and suggestions for how this could be used in philosophy or media studies courses."

# Remove