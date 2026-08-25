data Command
  = -- | @run@ command
    Run
      [FilePath]
      [Regex]
      Bool
      (Maybe LicenseType)
      [TemplateRef]
      [Text]
      (Maybe RunMode)
      Bool
      Bool
  | -- | @gen@ command
    Gen Bool (Maybe (LicenseType, FileType))
