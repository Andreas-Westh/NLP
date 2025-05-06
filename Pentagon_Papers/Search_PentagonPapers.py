from pathlib import Path  # pathlib makes it easier to work with file paths

# read in all the URLs from the file
with open("NLP/DALF25PentagonPapers/content.txt", "r") as f:
    urls = [line.strip() for line in f if line.strip()]  # strip removes spaces/newlines

# grab just the filenames (the part after the last slash in the URL)
pdf_filenames = [Path(url).name for url in urls]

# this is the folder where I have all the individual page .txt files
txt_folder = Path("NLP/DALF25PentagonPapers/text")

# grab all .txt files in that folder, including their full paths
# .glob("*.txt") just means “give me all the .txt files here”
all_txts = list(txt_folder.glob("*.txt"))

# from the .txt files, extract the original PDF name they came from
# like "Pentagon-Papers-Part-V-B-3a.pdf-182.png.txt" becomes "Pentagon-Papers-Part-V-B-3a.pdf"
found_pdf_basenames = {
    txt.name.split(".pdf-")[0] + ".pdf"  # keep only the base part
    for txt in all_txts
}

# loop through the PDFs I want to check
for pdf in pdf_filenames:
    # if this one isn't found in the .txt files, it's missing
    if pdf not in found_pdf_basenames:
        print(f"{pdf} is missing")

