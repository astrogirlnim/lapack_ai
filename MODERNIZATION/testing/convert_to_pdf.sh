#!/bin/bash

# ================================================================
# PROFESSIONAL PDF CONVERSION SCRIPT FOR TEST RESULTS
# ================================================================
#
# This script converts all markdown test result files to professional,
# well-formatted PDFs using pandoc with optimized styling for
# technical documentation.
#
# Features:
# - Professional layout with proper margins and typography
# - Syntax highlighting for code blocks
# - Proper table formatting
# - Headers and footers with document metadata
# - High-quality LaTeX rendering
# - Consistent styling across all documents
#
# Author: LAPACK AI Development Team
# Generated: Post-Phase 9.2 GPU implementation

set -e  # Exit on any error

echo "================================================================"
echo "PROFESSIONAL PDF CONVERSION FOR TEST RESULTS"
echo "================================================================"
echo ""

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SOURCE_DIR="$SCRIPT_DIR"
OUTPUT_DIR="$SOURCE_DIR/pdf_reports"
TEMP_DIR="$SOURCE_DIR/.temp_pdf_conversion"

# Create output and temp directories
mkdir -p "$OUTPUT_DIR"
mkdir -p "$TEMP_DIR"

echo "📁 Source Directory: $SOURCE_DIR"
echo "📁 Output Directory: $OUTPUT_DIR"
echo ""

# Check dependencies
echo "🔍 Checking dependencies..."
if ! command -v pandoc &> /dev/null; then
    echo "❌ ERROR: pandoc is not installed"
    echo "   Install with: brew install pandoc"
    exit 1
fi

# Check for LaTeX (required for high-quality PDF output)
if ! command -v pdflatex &> /dev/null; then
    echo "⚠️  WARNING: pdflatex not found. Installing BasicTeX (this may take a moment)..."
    if command -v brew &> /dev/null; then
        brew install --cask basictex
        echo "✅ BasicTeX installed. You may need to restart your terminal."
        echo "   After restart, run this script again."
        exit 0
    else
        echo "❌ ERROR: Please install LaTeX manually for high-quality PDF output"
        echo "   macOS: brew install --cask basictex"
        echo "   Ubuntu: sudo apt-get install texlive-latex-recommended"
        exit 1
    fi
fi

echo "✅ All dependencies found"
echo ""

# Create custom LaTeX template for professional styling
cat > "$TEMP_DIR/professional_template.tex" << 'EOF'
\documentclass[11pt,a4paper]{article}

% Professional packages
\usepackage[utf8]{inputenc}
\usepackage[margin=1in]{geometry}
\usepackage{fancyhdr}
\usepackage{graphicx}
\usepackage{xcolor}
\usepackage{listings}
\usepackage{booktabs}
\usepackage{longtable}
\usepackage{array}
\usepackage{hyperref}
\usepackage{titlesec}

% Professional color scheme
\definecolor{primaryblue}{RGB}{0,102,204}
\definecolor{darkgray}{RGB}{64,64,64}
\definecolor{lightgray}{RGB}{245,245,245}
\definecolor{codebackground}{RGB}{248,248,248}

% Configure hyperlinks
\hypersetup{
    colorlinks=true,
    linkcolor=primaryblue,
    urlcolor=primaryblue,
    citecolor=primaryblue,
    pdfborder={0 0 0}
}

% Professional typography
\usepackage{lmodern}
\usepackage[T1]{fontenc}

% Header and footer styling
\pagestyle{fancy}
\fancyhf{}
\fancyhead[L]{\fontsize{9}{11}\selectfont LAPACK AI AlphaTensor Implementation}
\fancyhead[R]{\fontsize{9}{11}\selectfont $title$}
\fancyfoot[L]{\fontsize{9}{11}\selectfont Generated: \today}
\fancyfoot[R]{\fontsize{9}{11}\selectfont Page \thepage}
\renewcommand{\headrulewidth}{0.5pt}
\renewcommand{\footrulewidth}{0.5pt}

% Section title styling
\titleformat{\section}
  {\Large\bfseries\color{primaryblue}}
  {\thesection}{1em}{}

\titleformat{\subsection}
  {\large\bfseries\color{darkgray}}
  {\thesubsection}{1em}{}

% Code block styling
\lstset{
    backgroundcolor=\color{codebackground},
    basicstyle=\footnotesize\ttfamily,
    frame=single,
    framerule=0.5pt,
    rulecolor=\color{lightgray},
    breaklines=true,
    breakatwhitespace=true,
    showstringspaces=false,
    tabsize=2,
    xleftmargin=0.5cm,
    xrightmargin=0.5cm,
    aboveskip=1em,
    belowskip=1em
}

% Table styling
\renewcommand{\arraystretch}{1.2}

% Title page customization
\makeatletter
\renewcommand{\maketitle}{
  \begin{titlepage}
    \centering
    \vspace*{2cm}

    {\Huge\bfseries\color{primaryblue} $title$ \par}
    \vspace{1cm}

    {\Large LAPACK AI AlphaTensor Implementation \par}
    \vspace{0.5cm}

    {\large Technical Report \par}
    \vspace{2cm}

    \rule{\textwidth}{0.5pt}
    \vspace{1cm}

    {\large Generated: \today \par}
    \vspace{0.5cm}

    {\normalsize Professional PDF Report \par}

    \vfill

    {\footnotesize
    This document contains comprehensive test results and analysis\\
    for the AlphaTensor matrix multiplication implementation.\\
    Generated automatically from markdown documentation.
    \par}

  \end{titlepage}
  \newpage
}
\makeatother

% Document content starts here
\begin{document}

$if(title)$
\title{$title$}
\maketitle
$endif$

$if(toc)$
\tableofcontents
\newpage
$endif$

$body$

\end{document}
EOF

# Function to extract title from markdown file
extract_title() {
    local file="$1"
    # Look for the first H1 heading
    local title=$(grep -m1 "^# " "$file" | sed 's/^# //' | sed 's/[#*`]//g' | xargs)
    if [ -z "$title" ]; then
        # Fallback to filename
        title=$(basename "$file" .md | sed 's/_/ /g' | sed 's/\b\w/\u&/g')
    fi
    echo "$title"
}

# Function to convert a single markdown file to PDF
convert_to_pdf() {
    local md_file="$1"
    local filename=$(basename "$md_file" .md)
    local title=$(extract_title "$md_file")
    local pdf_file="$OUTPUT_DIR/${filename}.pdf"

    echo "📄 Converting: $filename"
    echo "   Title: $title"

    # Enhanced pandoc conversion with professional options
    pandoc "$md_file" \
        --template="$TEMP_DIR/professional_template.tex" \
        --pdf-engine=pdflatex \
        --variable title="$title" \
        --variable toc=true \
        --variable geometry:margin=1in \
        --variable fontsize=11pt \
        --variable documentclass=article \
        --variable colorlinks=true \
        --listings \
        --number-sections \
        --toc \
        --toc-depth=3 \
        --highlight-style=tango \
        --wrap=auto \
        --standalone \
        -o "$pdf_file"

    if [ $? -eq 0 ]; then
        echo "   ✅ Successfully created: $(basename "$pdf_file")"
        echo "   📄 File size: $(du -h "$pdf_file" | cut -f1)"
    else
        echo "   ❌ Failed to convert: $filename"
        return 1
    fi
    echo ""
}

# Get list of markdown files to convert
echo "🔍 Finding markdown files to convert..."
md_files=($(find "$SOURCE_DIR" -maxdepth 1 -name "*.md" -type f | sort))

if [ ${#md_files[@]} -eq 0 ]; then
    echo "❌ No markdown files found in $SOURCE_DIR"
    exit 1
fi

echo "📋 Found ${#md_files[@]} markdown files:"
for file in "${md_files[@]}"; do
    echo "   • $(basename "$file")"
done
echo ""

# Convert each file
echo "🚀 Starting PDF conversion..."
echo ""

success_count=0
total_count=${#md_files[@]}

for md_file in "${md_files[@]}"; do
    if convert_to_pdf "$md_file"; then
        ((success_count++))
    fi
done

# Summary
echo "================================================================"
echo "CONVERSION SUMMARY"
echo "================================================================"
echo "📊 Successfully converted: $success_count/$total_count files"
echo "📁 Output directory: $OUTPUT_DIR"
echo ""

if [ $success_count -eq $total_count ]; then
    echo "🎉 All files converted successfully!"
    echo ""
    echo "📋 Generated PDF files:"
    for pdf in "$OUTPUT_DIR"/*.pdf; do
        if [ -f "$pdf" ]; then
            echo "   • $(basename "$pdf") ($(du -h "$pdf" | cut -f1))"
        fi
    done
    echo ""
    echo "💡 Professional features included:"
    echo "   ✅ Custom typography and layout"
    echo "   ✅ Syntax highlighted code blocks"
    echo "   ✅ Professional headers and footers"
    echo "   ✅ Table of contents with page numbers"
    echo "   ✅ Proper section numbering"
    echo "   ✅ High-quality LaTeX rendering"
    echo "   ✅ Clickable cross-references and links"
    echo ""
    echo "📖 To view PDFs: open $OUTPUT_DIR"
else
    echo "⚠️  Some conversions failed. Check the output above for details."
fi

# Cleanup
rm -rf "$TEMP_DIR"

echo ""
echo "✨ Professional PDF conversion complete!"
echo "================================================================"
