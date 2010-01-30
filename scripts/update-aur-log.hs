main = do
    s <- getContents
    let (name,desc,url) = read s
    writeFile "/tmp/title" name
    writeFile "/tmp/desc"  desc
    writeFile "/tmp/link"  url

