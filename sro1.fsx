// ===== Готовый фрагмент: чтение CSV (скопируйте без изменений) =====
// readRows "data.csv" — читает файл рядом со скриптом,
// пропускает строку заголовков и пустые строки,
// возвращает список строк; каждая строка — массив ячеек.
let readRows (fileName: string) =
    let path = System.IO.Path.Combine(__SOURCE_DIRECTORY__, fileName)
    System.IO.File.ReadAllLines path
    |> Array.toList
    |> List.tail
    |> List.filter (fun line -> line.Trim() <> "")
    |> List.map (fun line ->
        line.Split ';' |> Array.map (fun cell -> cell.Trim()))
 
// Число с точкой или запятой: "3.5" и "3,5" -> 3.5
let toFloat (text: string) = float (text.Replace(',', '.'))
 
// Дата в виде "2026-10-05" или "05.10.2026" -> System.DateTime
let toDate (text: string) =
    System.DateTime.ParseExact(text, [| "yyyy-MM-dd"; "dd.MM.yyyy" |], null,
                               System.Globalization.DateTimeStyles.None)
// Дата для печати: showDate d -> "05.10.2026"
let showDate (date: System.DateTime) = date.ToString "dd.MM.yyyy"
// ===== Конец готового фрагмента =====

type Session =
    { Date: System.DateTime
      StartTime: string
      Subject: string
      Place: string
      Minutes: int
      Productivity: int
      Distractions: int }

let parseSession (cells: string[]) =
    { Date = toDate cells[0]
      StartTime = cells[1]
      Subject = cells[2]
      Place = cells[3]
      Minutes = int cells[4]
      Productivity = int cells[5]
      Distractions = int cells[6] }

let sessions = readRows "data.csv" |> List.map parseSession
let hoursBySubject ss =
    ss
    |> List.groupBy (fun s -> s.Subject)
    |> List.map (fun (subj, lst) -> (subj, (float (lst |> List.sumBy (fun s -> s.Minutes))) / 60.0))
    |> List.sortByDescending (fun (_, hours) -> hours)

let usefulMinutes (s: Session) =
    (float s.Minutes * float s.Productivity) / 5.0

let usefulBySubject ss =
    ss
    |> List.groupBy (fun s -> s.Subject)
    |> List.map (fun (subj, lst) ->
        let totalMins = float (lst |> List.sumBy (fun s -> s.Minutes))
        let totalUseful = lst |> List.sumBy usefulMinutes
        let share = if totalMins > 0.0 then (totalUseful / totalMins) * 100.0 else 0.0
        (subj, share))

let focusByPlace ss =
    ss
    |> List.groupBy (fun s -> s.Place)
    |> List.map (fun (place, lst) ->
        let avgProd = lst |> List.averageBy (fun s -> float s.Productivity)
        (place, avgProd))
    |> List.sortByDescending (fun (_, avg) -> avg)

let distractionsPerHour ss =
    let totalMins = float (ss |> List.sumBy (fun s -> s.Minutes))
    let totalDistr = float (ss |> List.sumBy (fun s -> s.Distractions))
    if totalMins > 0.0 then (totalDistr / totalMins) * 60.0 else 0.0

let distractionsByPlace ss =
    ss
    |> List.groupBy (fun s -> s.Place)
    |> List.map (fun (place, lst) -> (place, distractionsPerHour lst))

let partOfDay (s: Session) =
    let hour = int (s.StartTime.Substring(0, 2))
    if hour < 12 then "Утро"
    elif hour < 18 then "День"
    else "Вечер"

let focusByPartOfDay ss =
    ss
    |> List.groupBy partOfDay
    |> List.map (fun (part, lst) ->
        let avgProd = lst |> List.averageBy (fun s -> float s.Productivity)
        (part, avgProd))

let busiestDay ss =
    ss
    |> List.groupBy (fun s -> s.Date)
    |> List.map (fun (date, lst) -> (date, lst |> List.sumBy (fun s -> s.Minutes)))
    |> List.maxBy (fun (_, mins) -> mins)

let distractionsBySubject ss =
    ss
    |> List.groupBy (fun s -> s.Subject)
    |> List.map (fun (subj, lst) -> (subj, lst |> List.sumBy (fun s -> s.Distractions)))
    |> List.sortByDescending (fun (_, distr) -> distr)

let printReport ss =
    let totalSessions = List.length ss
    let totalHours = (float (ss |> List.sumBy (fun s -> s.Minutes))) / 60.0
    
    printfn "=== Учебное время ==="
    printfn "Сессий: %d, всего: %.1f ч" totalSessions totalHours
    
    printfn "Часы по предметам:"
    ss |> hoursBySubject |> List.iter (fun (subj, h) -> printfn "  %s: %.1f ч" subj h)
    
    printfn "Полезное время (доля в %%):"
    ss |> usefulBySubject |> List.iter (fun (subj, share) -> printfn "  %s: %.0f%%" subj share)
    
    printfn "Средняя продуктивность по местам:"
    ss |> focusByPlace |> List.iter (fun (place, prod) -> printfn "  %s: %.1f" place prod)
    
    printfn "Отвлечений в час:"
    ss |> distractionsByPlace |> List.iter (fun (place, dist) -> printfn "  %s: %.1f" place dist)
    
    printfn "Продуктивность по времени суток:"
    ss |> focusByPartOfDay |> List.iter (fun (part, prod) -> printfn "  %s: %.1f" part prod)
    
    let (bDate, bMins) = busiestDay ss
    printfn "Самый учебный день: %s (%d мин)" (showDate bDate) bMins

printReport sessions