for png_file in *.png; do
    # Get the base name without the extension
    base_name="${png_file%.png}"
    
    # Convert the PNG file to a PDF file with the same base name
    convert "$png_file" "${base_name}.pdf"
    
    rm "$png_file"
done

# to merge all pdfs :

pdfunite batch_*.pdf all.pdf
find . -maxdepth 1 -type f -name "*.pdf" ! -name "all.pdf" -exec rm -v {} +
