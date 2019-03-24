for /r test/schema %%f in (*.xsd) do java jaxp.SourceValidator -xsd11 -a %%f
PAUSE