#!/bin/bash

# Create an array of all PDF files in the current directory
pdf_files=(*.pdf)

# Set the batch size
batch_size=10

# Calculate the number of batches needed
num_batches=$(((${#pdf_files[@]} + $batch_size - 1) / $batch_size))

# Loop through each batch
for ((i=0; i<$num_batches; i++)); do
    # Determine start and end index for the current batch
    start_index=$((i * $batch_size))
    end_index=$((start_index + batch_size - 1))

    # Ensure end index doesn't exceed the total number of files
    if ((end_index >= ${#pdf_files[@]})); then
        end_index=$((${#pdf_files[@]} - 1))
    fi

    # Extract the PDF files for the current batch
    batch=("${pdf_files[@]:start_index:end_index-start_index+1}")

    # Merge the PDF files in the current batch
    pdfunite "${batch[@]}" "batch_$i.pdf"
done

# Merge all batch files into one big PDF
pdfunite batch_*.pdf all.pdf

# Clean up intermediate batch files and single PDFs
rm batch_*.pdf
find . -maxdepth 1 -type f ! -name 'all.pdf' ! -name 'merge_pdfs.sh' -exec rm -v {} +

