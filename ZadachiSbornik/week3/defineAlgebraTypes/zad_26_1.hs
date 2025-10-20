--26.1. Да се дефинира тип DayOfWeek, представящ ден от седмицата. За типа
--да се реализират възможност за:
-- Въвеждане и извеждане
-- Сравнение на дни с ==,<,>
-- Намиране на следващ и предишен ден от седмицата
-- Преобразуване от и до число
-- Създаване на интервал от дни
--Съответните възможности да се добавят по два начина:
--  Чрез нарочни функции за целта
--  Чрез наследяване на базови класове

data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving(Read,Show,Eq,Ord,Enum)

nextDay :: DayOfWeek -> DayOfWeek
nextDay Sunday = Monday
nextDay x = succ x

prevDay :: DayOfWeek -> DayOfWeek
prevDay Monday = Sunday
prevDay x = pred x

dayToInt :: DayOfWeek -> Int
dayToInt x = fromEnum x + 1

intToDay :: Int -> DayOfWeek
intToDay x = toEnum (x - 1)

daysInterval :: DayOfWeek -> DayOfWeek -> [DayOfWeek]
daysInterval start end
    | start <= end = [start .. end]
    | otherwise = [start .. Sunday] ++ [Monday .. end]