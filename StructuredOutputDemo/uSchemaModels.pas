unit uSchemaModels;

interface

uses
  System.Generics.Collections,
  PasDantic.Attributes;

type
  /// Represents a single step in a recipe
  TRecipeStep = class
  public
    [Required]
    [Range(1, 100)]
    StepNumber: Integer;

    [Required]
    [StringLength(5, 500)]
    Instruction: string;

    [Optional]
    DurationMinutes: Integer;
  end;

  /// Structured recipe output from LLM
  TRecipe = class
  public
    [Required]
    [StringLength(3, 100)]
    Name: string;

    [Required]
    [StringLength(10, 500)]
    Description: string;

    [Required]
    [Range(1, 20)]
    Servings: Integer;

    [Required]
    [Range(1, 600)]
    PrepTimeMinutes: Integer;

    [Required]
    [Range(0, 600)]
    CookTimeMinutes: Integer;

    [Required]
    [AllowedValues('Easy,Medium,Hard')]
    Difficulty: string;

    [Required]
    Steps: TObjectList<TRecipeStep>;
  end;

  /// Represents a single item in a movie review
  TMovieReview = class
  public
    [Required]
    [StringLength(1, 200)]
    Title: string;

    [Required]
    [Range(1900, 2030)]
    Year: Integer;

    [Required]
    [StringLength(3, 50)]
    Genre: string;

    [Required]
    [Range(1, 10)]
    Rating: Integer;

    [Required]
    [StringLength(20, 500)]
    Summary: string;

    [Required]
    [AllowedValues('Positive,Negative,Mixed')]
    Sentiment: string;
  end;

  /// Contact information extraction
  TContactInfo = class
  public
    [Required]
    [StringLength(1, 100)]
    FullName: string;

    [Optional]
    [Email]
    EmailAddress: string;

    [Optional]
    [StringLength(5, 30)]
    PhoneNumber: string;

    [Optional]
    [StringLength(2, 100)]
    Company: string;

    [Optional]
    [StringLength(2, 100)]
    JobTitle: string;
  end;

implementation

end.
