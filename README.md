# mem_app
organize photos and memories

# process warning messages

- unique directories with errors copying

```
grep "\\-\\- FALSE \\-\\- FALSE \\-\\- FALSE \\-\\- FALSE" out.1 | cut -d " " -f9 | cut -d "/" -f1 | uniq -c
```

- errors.log

```
grep "\\-\\- FALSE \\-\\- FALSE \\-\\- FALSE \\-\\- FALSE" out.* | cut -d " " -f9-11 > errors.log
```

