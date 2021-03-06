module TeamCity.Flakies.FileSystem

open System
open System.IO

type String with
    static member Random() =
        let id = Guid.NewGuid()
        id.ToString()

type FilePath private (path) =

    static member New path =
        FilePath(System.IO.Path.GetFullPath path)

    member __.AsFileInfo = FileInfo path

    member x.LastWriteTimeUtc
        with get () = x.AsFileInfo.LastWriteTimeUtc
        and set (time) = x.AsFileInfo.LastAccessTimeUtc <- time

    member __.Path = path
    member __.Name = Path.GetFileName path
    member __.Exists = File.Exists path

    member __.WithExtension ext =
        Path.ChangeExtension(path, ext) |> FilePath.New

    member __.WriteAllText contents = File.WriteAllText(path, contents)
    member __.Delete() = File.Delete path

    member __.MoveTo(dest: FilePath, ?overwrite0: bool) =
        let overwrite = defaultArg overwrite0 false

        if overwrite && dest.Exists then
            dest.Delete()

        File.Move(path, dest.Path)

    member __.ReadAllText() =
        //File.ReadAllText(path) // do not support files already opened for writing
        use fs = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
        use sr = new StreamReader(fs)
        sr.ReadToEnd()

    member __.DeleteIgnoringErrors() =
        try
            File.Delete path
        with _ -> ()

    member x.Touch() = x.LastWriteTimeUtc <- DateTime.UtcNow
    member __.Directory = Path.GetDirectoryName(path) |> DirPath.New
    member x.DeleteIfExists() = if x.Exists then x.Delete()

    interface IComparable<FilePath> with
        member x.CompareTo other =
            StringComparer.OrdinalIgnoreCase.Compare(x.Path, other.Path)

    override __.ToString() = path

    override __.GetHashCode() =
        path.GetHashCode(StringComparison.OrdinalIgnoreCase)

    override __.Equals(b) =
        match b with
        | :? FilePath as f -> StringComparer.OrdinalIgnoreCase.Equals(path, f.Path)
        | _ -> false

and DirPath private (path) =

    static member New path =
        let separator = Path.DirectorySeparatorChar

        DirPath(
            System.IO.Path.GetFullPath(path).TrimEnd(separator)
            + separator.ToString()
        )

    member __.Path = path
    member __.Name = Path.GetFileName(Path.TrimEndingDirectorySeparator path)

    member __.FileInDir relative =
        Path.Combine(path, relative) |> FilePath.New

    override __.ToString() = ""

    member __.Exists = Directory.Exists path

    member x.CheckExists() =
        if not x.Exists then
            failwith (sprintf "'%s' not found" path)

    member __.DeleteIgnoringErrors() =
        try
            Directory.Delete(path, true)
        with e -> printfn "%s" (e.ToString())

    static member Temp = Path.GetTempPath() |> DirPath.New

    member __.Create() =
        Directory.CreateDirectory path |> ignore

    member __.CreateNew() =
        let temp =
            Path.TrimEndingDirectorySeparator path
            + sprintf ".%s" (String.Random())
            |> DirPath.New

        try
            temp.Create()
            Directory.Move(temp.Path, path)
        finally
            if temp.Exists then
                temp.DeleteIgnoringErrors()

    member x.CreateUnique?name =
        match name with
        | Some name ->
            let mutable index = 0

            let rec create () =
                let dirName =
                    if index = 0 then
                        name
                    else
                        name + sprintf "-%d" index

                let temp = Path.Combine(path, dirName) |> DirPath.New

                try
                    temp.CreateNew()
                    temp
                with e ->
                    if index >= 1000 then
                        failwith (
                            Path.Combine(path, name)
                            |> sprintf "Failed to create %s"
                        )
                    else
                        index <- index + 1
                        create ()

            create ()
        | _ ->
            let temp =
                Path.TrimEndingDirectorySeparator path
                + sprintf ".%s" (String.Random())
                |> DirPath.New

            temp.CreateNew()
            temp

    static member createTemp(?name) =
        let name = defaultArg name (String.Random())
        let dir = DirPath.Temp.CreateUnique name

        { new IDisposable with
            member x.Dispose() = dir.DeleteIgnoringErrors() }

type TempDir private (dir) =
    new(?name) =
        let parent = DirPath.Temp

        let dir =
            match name with
            | Some name -> parent.CreateUnique name
            | _ -> parent.CreateUnique()

        new TempDir(dir)

    member __.Dir = dir

    interface IDisposable with
        member __.Dispose() = dir.DeleteIgnoringErrors()

    override x.ToString() = dir.Path

type TempFile private (file) =
    new(?name, ?ext) =
        let name =
            match name with
            | Some name -> sprintf "%s-%s" name (String.Random())
            | _ -> String.Random()

        let ext = defaultArg ext ".tmp"
        let file = DirPath.Temp.FileInDir(name + ext)
        new TempFile(file)

    member __.File = file

    interface IDisposable with
        member __.Dispose() = file.DeleteIgnoringErrors()

    override x.ToString() = file.Path
