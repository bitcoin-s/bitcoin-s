-- Fix dummy event descriptor to be a parsable one
UPDATE events SET event_descriptor_tlv = 'fdd8060800010564756d6d79' WHERE event_descriptor_tlv = 'fdd806090001000564756d6d79';
