USE Projects;

SELECT *
FROM Year2018; -- 21996


-- All the files seem to have the same type of data so combining all the tables

WITH all_hotels AS(
		SELECT *
		FROM Year2018
		UNION
		SELECT *
		FROM Year2019
		UNION
		SELECT *
		FROM Year2020)
		
SELECT *
INTO all_hotels
FROM all_hotels;



-- Cleaning data

SELECT *
FROM all_hotels -- 100712
WHERE adults < 1 --385 
AND reservation_status ='Canceled'; --93 (is_cancelled = 1)

SELECT *
FROM all_hotels
WHERE total_of_special_requests < 1; --15285




-- EDA: Required Categorical Variables: hotel, reservation_status, reserved_room_type, assigned_room_type, deposit_type, customer_type

SELECT DISTINCT hotel
FROM all_hotels; -- City Hotel/Resort Hotel

SELECT DISTINCT reservation_status
FROM all_hotels; --Check-Out/No-Show/Canceled


SELECT DISTINCT reserved_room_type
FROM all_hotels
ORDER BY reserved_room_type; -- A:H,L,P

SELECT DISTINCT assigned_room_type
FROM all_hotels
ORDER BY assigned_room_type; -- A:I,K,L,P

SELECT DISTINCT customer_type
FROM all_hotels; -- Group/Contract/Transient/Transient-Party




--Analyzing if the Customers got their ideal room type

SELECT COUNT(*) AS ideal_room
FROM all_hotels
WHERE reserved_room_type = assigned_room_type; -- 84607 rows




--Analyzing if the Customers got better rooms

SELECT COUNT(*) AS better_room
FROM all_hotels
WHERE ASCII(reserved_room_type) > ASCII(assigned_room_type); -- 695

SELECT *
FROM all_hotels
WHERE ASCII(reserved_room_type) > ASCII(assigned_room_type);



-- Total Revenue check

SELECT arrival_date_year,
	   hotel,
	   SUM((stays_in_week_nights+stays_in_weekend_nights)*adr) as all_revenue
FROM all_hotels
GROUP BY arrival_date_year, hotel
ORDER BY arrival_date_year, hotel;


SELECT *
FROM Revenue;

-- Incase the data required is Revenue excluding Canceled

SELECT arrival_date_year,
	   hotel,
	   SUM((stays_in_week_nights+stays_in_weekend_nights)*adr) as revenue
INTO ActualRevenue
FROM all_hotels
WHERE reservation_status = 'Canceled'
GROUP BY arrival_date_year, hotel
ORDER BY arrival_date_year, hotel;

SELECT *
FROM ActualRevenue;

-- Car parking

SELECT DISTINCT required_car_parking_spaces
FROM all_hotels;

SELECT COUNT(required_car_parking_spaces)
FROM all_hotels
WHERE required_car_parking_spaces > 0; -- 8640


-- Adding a column for Seasonal Trends

ALTER TABLE all_hotels
ADD Seasons varchar(20);

SELECT *
FROM all_hotels;

-- December, January, February (winter), March, April, May (spring), June, July, August (summer), September, October, November (autumn).

SELECT DISTINCT arrival_date_month
FROM all_hotels;

SELECT arrival_date_month
FROM all_hotels
WHERE arrival_date_month IS NULL; -- None

UPDATE all_hotels
SET Seasons = 'WINTER'
WHERE arrival_date_month IN ('December', 'January', 'February');

UPDATE all_hotels
SET Seasons =
	CASE
		WHEN arrival_date_month IN ('March', 'April', 'May') THEN 'Spring'
		WHEN arrival_date_month IN ('June', 'July', 'August') THEN 'Summer'
		ELSE 'Autumn'
	END;

-- Analyzing seasonal trends

SELECT customer_type, Seasons, SUM((stays_in_week_nights+stays_in_weekend_nights)*adr) as revenue
FROM all_hotels
GROUP BY Seasons, customer_type
ORDER BY customer_type;

SELECT *
FROM SeasonalRevenue;

SELECT *
FROM SeasonalCust;

SELECT *
FROM market_segment;

SELECT *
FROM meal_cost;

-- Joining the rest of the required data

SELECT *
FROM all_hotels ah
LEFT JOIN market_segment ms
ON ah.market_segment = ms.market_segment
LEFT JOIN meal_cost mc
ON ah.meal = mc.meal;

