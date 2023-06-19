# Extract restaurants in Phoenix (public5)
# Number observations: 4004

SELECT * 
FROM public5.businesstable
WHERE city LIKE 'Phoenix' and categories LIKE '%Restaurant%';

#---------------------------------------


# Outer full join business_table with checkin_table 
# Number observations: 4004

SELECT business_tb.business_id AS business_id_businesstable , checkin_tb.business_id AS business_id_checkintable, date
FROM public5.businesstable AS business_tb
FULL OUTER JOIN public5.checkintable AS checkin_tb
ON business_tb.business_id = checkin_tb.business_id
WHERE city LIKE 'Phoenix' and categories LIKE '%Restaurant%';

#----------------------------------------------

 # Select reviews from Phoenix's restaurants
 
SELECT business_tb.city,
business_tb.business_id AS business_id_businesstable,
review_tb.business_id AS business_id_review,
review_tb.review_id,review_tb.user_id, review_tb.stars, review_tb.date,
review_tb.text, review_tb.useful, review_tb.funny, review_tb.cool

FROM public5.businesstable AS business_tb
INNER JOIN public5.reviewtable AS review_tb
ON business_tb.business_id = review_tb.business_id
WHERE business_tb.city LIKE 'Phoenix' and business_tb.categories LIKE '%Restaurant%';


# (Optional) Number of reviews for each Phoenix's restuarant

SELECT business_tb.business_id, COUNT(review_tb.review_id)

FROM public5.businesstable AS business_tb
INNER JOIN public5.reviewtable AS review_tb
ON business_tb.business_id = review_tb.business_id
WHERE business_tb.city LIKE 'Phoenix' and business_tb.categories LIKE '%Restaurant%'
GROUP BY business_tb.business_id
ORDER BY COUNT(review_tb.review_id) DESC;


#--------------------------------------------

# Get photos of Phoenix's restaurants

SELECT business_tb.city, photo_tb.photo_id, business_tb.business_id, photo_tb.caption, photo_tb.label 
FROM public5.businesstable AS business_tb
INNER JOIN public5.phototable AS photo_tb
ON business_tb.business_id = photo_tb.business_id
WHERE business_tb.city LIKE 'Phoenix' and business_tb.categories LIKE '%Restaurant%';

# (Optional) Number of pictures per Phoenix's rest.

SELECT business_tb.business_id, COUNT(photo_tb.photo_id) 
FROM public5.businesstable AS business_tb
INNER JOIN public5.phototable AS photo_tb
ON business_tb.business_id = photo_tb.business_id
WHERE business_tb.city LIKE 'Phoenix' and business_tb.categories LIKE '%Restaurant%'
GROUP BY business_tb.business_id
ORDER BY COUNT(photo_tb.photo_id) DESC;

#-------------------------------------

# Get tips of Phoenix's restaurants

SELECT business_tb.city, tips_tb.user_id, business_tb.business_id, tips_tb.compliment_count, tips_tb.date, tips_tb.text
FROM public5.businesstable AS business_tb
INNER JOIN public5.tipstable AS tips_tb
ON business_tb.business_id = tips_tb.business_id
WHERE business_tb.city LIKE 'Phoenix' and business_tb.categories LIKE '%Restaurant%';

# (Optional) Number of tips per restaurant

SELECT business_tb.business_id, COUNT(*)
FROM public5.businesstable AS business_tb
INNER JOIN public5.tipstable AS tips_tb
ON business_tb.business_id = tips_tb.business_id
WHERE business_tb.city LIKE 'Phoenix' and business_tb.categories LIKE '%Restaurant%'
GROUP BY business_tb.business_id
ORDER BY COUNT(*) DESC;

 #----------------------------------------

 # Get users that wrote reviews on Phoenix's restaurants

SELECT users_tb.*
FROM public5.businesstable AS business_tb
JOIN public5.reviewtable AS review_tb
ON business_tb.business_id = review_tb.business_id
JOIN public5.userstable AS users_tb
ON review_tb.user_id = users_tb.user_id
WHERE business_tb.city LIKE 'Phoenix' and business_tb.categories LIKE '%Restaurant%';
